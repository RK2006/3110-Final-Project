type config = {
  width : int;
  height : int;
  margin_left : int;
  margin_right : int;
  margin_top : int;
  margin_bottom : int;
  title : string;
}

let default_config =
  {
    width = 900;
    height = 500;
    margin_left = 80;
    margin_right = 30;
    margin_top = 50;
    margin_bottom = 60;
    title = "PnL Curve";
  }

let plot_width c = c.width - c.margin_left - c.margin_right
let plot_height c = c.height - c.margin_top - c.margin_bottom

let bounds curve =
  let n = Array.length curve in
  if n = 0 then (0.0, 0.0)
  else
    Array.fold_left
      (fun (mn, mx) v -> (Float.min mn v, Float.max mx v))
      (curve.(0), curve.(0))
      curve

let nice_pad ~min_v ~max_v =
  if min_v = max_v then (min_v -. 1.0, max_v +. 1.0)
  else
    let pad = (max_v -. min_v) *. 0.05 in
    (min_v -. pad, max_v +. pad)

let xpos c ~n_points i =
  let plot_w = float_of_int (plot_width c) in
  if n_points <= 1 then float_of_int c.margin_left +. (plot_w /. 2.0)
  else
    let frac = float_of_int i /. float_of_int (n_points - 1) in
    float_of_int c.margin_left +. (plot_w *. frac)

let ypos c ~min_v ~max_v v =
  let plot_h = float_of_int (plot_height c) in
  let frac =
    if max_v = min_v then 0.5 else (v -. min_v) /. (max_v -. min_v)
  in
  float_of_int c.margin_top +. plot_h -. (plot_h *. frac)

let buf_fmt buf fmt = Printf.ksprintf (Buffer.add_string buf) fmt

let escape_xml s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun ch ->
      match ch with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | _ -> Buffer.add_char buf ch)
    s;
  Buffer.contents buf

let header buf c =
  buf_fmt buf
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" \
     width=\"%d\" height=\"%d\" font-family=\"sans-serif\">\n"
    c.width c.height c.width c.height

let footer buf = Buffer.add_string buf "</svg>\n"

let style buf =
  Buffer.add_string buf
    "<style>\n\
    \  .frame { fill: white; stroke: #444; stroke-width: 1; }\n\
    \  .axis text { fill: #222; font-size: 11px; }\n\
    \  .grid { stroke: #ddd; stroke-width: 1; }\n\
    \  .pnl { fill: none; stroke: #1f77b4; stroke-width: 1.5; }\n\
    \  .zero { stroke: #888; stroke-width: 1; stroke-dasharray: 4 3; }\n\
    \  .buy { fill: #2ca02c; }\n\
    \  .sell { fill: #d62728; }\n\
    \  .title { font-size: 16px; font-weight: bold; fill: #111; }\n\
    \  .legend-bg { fill: white; stroke: #aaa; stroke-width: 1; }\n\
    \  .legend { font-size: 11px; fill: #222; }\n\
    </style>\n"

let title buf c =
  buf_fmt buf
    "<text class=\"title\" x=\"%d\" y=\"%d\" text-anchor=\"middle\">%s</text>\n"
    (c.margin_left + (plot_width c / 2))
    (c.margin_top - 18) (escape_xml c.title)

let plot_frame buf c =
  buf_fmt buf
    "<rect class=\"frame\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" />\n"
    c.margin_left c.margin_top (plot_width c) (plot_height c)

let format_dollars v =
  let abs_v = Float.abs v in
  if abs_v >= 1000.0 then Printf.sprintf "$%.0f" v
  else if abs_v >= 100.0 then Printf.sprintf "$%.1f" v
  else Printf.sprintf "$%.2f" v

let x_axis buf c ~n_points =
  let n_ticks = 5 in
  let y_top = float_of_int c.margin_top in
  let y_bot = float_of_int (c.margin_top + plot_height c) in
  for k = 0 to n_ticks - 1 do
    let frac = float_of_int k /. float_of_int (n_ticks - 1) in
    let step_idx =
      if n_points <= 1 then 0
      else int_of_float (frac *. float_of_int (n_points - 1))
    in
    let x = xpos c ~n_points step_idx in
    buf_fmt buf
      "<line class=\"grid\" x1=\"%.1f\" y1=\"%.1f\" x2=\"%.1f\" y2=\"%.1f\" />\n"
      x y_top x y_bot;
    buf_fmt buf
      "<text class=\"axis\" x=\"%.1f\" y=\"%.1f\" \
       text-anchor=\"middle\">%d</text>\n"
      x (y_bot +. 16.0) step_idx
  done;
  buf_fmt buf
    "<text class=\"axis\" x=\"%d\" y=\"%d\" text-anchor=\"middle\">step</text>\n"
    (c.margin_left + (plot_width c / 2))
    (c.height - 8)

let y_axis buf c ~min_v ~max_v =
  let n_ticks = 5 in
  let x_left = float_of_int c.margin_left in
  let x_right = float_of_int (c.margin_left + plot_width c) in
  for k = 0 to n_ticks - 1 do
    let frac = float_of_int k /. float_of_int (n_ticks - 1) in
    let v = min_v +. (frac *. (max_v -. min_v)) in
    let y = ypos c ~min_v ~max_v v in
    buf_fmt buf
      "<line class=\"grid\" x1=\"%.1f\" y1=\"%.1f\" x2=\"%.1f\" y2=\"%.1f\" />\n"
      x_left y x_right y;
    buf_fmt buf
      "<text class=\"axis\" x=\"%.1f\" y=\"%.1f\" \
       text-anchor=\"end\">%s</text>\n"
      (x_left -. 6.0) (y +. 4.0) (format_dollars v)
  done;
  let label_x = 22 in
  let label_y = c.margin_top + (plot_height c / 2) in
  buf_fmt buf
    "<text class=\"axis\" x=\"%d\" y=\"%d\" text-anchor=\"middle\" \
     transform=\"rotate(-90 %d %d)\">PnL ($)</text>\n"
    label_x label_y label_x label_y

let zero_line buf c ~min_v ~max_v =
  if min_v <= 0.0 && 0.0 <= max_v then
    let y = ypos c ~min_v ~max_v 0.0 in
    let x_left = float_of_int c.margin_left in
    let x_right = float_of_int (c.margin_left + plot_width c) in
    buf_fmt buf
      "<line class=\"zero\" x1=\"%.1f\" y1=\"%.1f\" x2=\"%.1f\" y2=\"%.1f\" />\n"
      x_left y x_right y

let pnl_polyline buf c curve ~min_v ~max_v =
  let n = Array.length curve in
  if n = 0 then ()
  else begin
    Buffer.add_string buf "<polyline class=\"pnl\" points=\"";
    Array.iteri
      (fun i v ->
        let x = xpos c ~n_points:n i in
        let y = ypos c ~min_v ~max_v v in
        if i > 0 then Buffer.add_char buf ' ';
        buf_fmt buf "%.2f,%.2f" x y)
      curve;
    Buffer.add_string buf "\" />\n"
  end

let triangle_up buf ~cx ~cy ~size ~clazz =
  let half = size /. 2.0 in
  buf_fmt buf
    "<polygon class=\"%s\" points=\"%.1f,%.1f %.1f,%.1f %.1f,%.1f\" />\n"
    clazz cx (cy -. half) (cx -. half) (cy +. half) (cx +. half) (cy +. half)

let triangle_down buf ~cx ~cy ~size ~clazz =
  let half = size /. 2.0 in
  buf_fmt buf
    "<polygon class=\"%s\" points=\"%.1f,%.1f %.1f,%.1f %.1f,%.1f\" />\n"
    clazz cx (cy +. half) (cx -. half) (cy -. half) (cx +. half) (cy -. half)

let trade_markers buf c curve actions ~min_v ~max_v =
  match actions with
  | None -> ()
  | Some actions ->
      let n = Array.length curve in
      Array.iteri
        (fun i action ->
          let step_idx = i + 1 in
          if step_idx >= n then ()
          else
            let x = xpos c ~n_points:n step_idx in
            let y = ypos c ~min_v ~max_v curve.(step_idx) in
            match (action : Environment.action) with
            | Environment.Buy ->
                triangle_up buf ~cx:x ~cy:y ~size:7.0 ~clazz:"buy"
            | Environment.Sell ->
                triangle_down buf ~cx:x ~cy:y ~size:7.0 ~clazz:"sell"
            | Environment.Hold -> ())
        actions

let legend buf c ~has_actions =
  let entries = if has_actions then 3 else 1 in
  let row_h = 16 in
  let pad = 8 in
  let lw = 130 in
  let lh = (entries * row_h) + (2 * pad) in
  let lx = c.margin_left + plot_width c - lw - 8 in
  let ly = c.margin_top + 8 in
  buf_fmt buf
    "<rect class=\"legend-bg\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" />\n"
    lx ly lw lh;
  let row_y = ly + pad in
  buf_fmt buf
    "<line class=\"pnl\" x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" />\n"
    (lx + pad) (row_y + 6)
    (lx + pad + 24)
    (row_y + 6);
  buf_fmt buf
    "<text class=\"legend\" x=\"%d\" y=\"%d\">PnL</text>\n"
    (lx + pad + 32)
    (row_y + 10);
  if has_actions then begin
    let row_y = row_y + row_h in
    let cx = float_of_int (lx + pad + 12) in
    let cy = float_of_int (row_y + 6) in
    triangle_up buf ~cx ~cy ~size:8.0 ~clazz:"buy";
    buf_fmt buf
      "<text class=\"legend\" x=\"%d\" y=\"%d\">Buy</text>\n"
      (lx + pad + 32)
      (row_y + 10);
    let row_y = row_y + row_h in
    let cy = float_of_int (row_y + 6) in
    triangle_down buf ~cx ~cy ~size:8.0 ~clazz:"sell";
    buf_fmt buf
      "<text class=\"legend\" x=\"%d\" y=\"%d\">Sell</text>\n"
      (lx + pad + 32)
      (row_y + 10)
  end

let render ?(config = default_config) ?actions curve =
  let buf = Buffer.create 4096 in
  let raw_min, raw_max = bounds curve in
  let min_v, max_v = nice_pad ~min_v:raw_min ~max_v:raw_max in
  header buf config;
  style buf;
  title buf config;
  plot_frame buf config;
  x_axis buf config ~n_points:(Array.length curve);
  y_axis buf config ~min_v ~max_v;
  zero_line buf config ~min_v ~max_v;
  pnl_polyline buf config curve ~min_v ~max_v;
  trade_markers buf config curve actions ~min_v ~max_v;
  legend buf config ~has_actions:(actions <> None);
  footer buf;
  Buffer.contents buf

let write ?config ?actions ~path curve =
  let oc = open_out path in
  output_string oc (render ?config ?actions curve);
  close_out oc
