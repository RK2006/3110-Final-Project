let write_csv curve path =
  let oc = open_out path in
  output_string oc "step,pnl\n";
  Array.iteri (fun step pnl -> Printf.fprintf oc "%d,%.10f\n" step pnl) curve;
  close_out oc

let clamp value low high = max low (min high value)

let sampled_curve curve ~width =
  let n = Array.length curve in
  if n = 0 then [||]
  else if n = 1 || width <= 1 then [| curve.(0) |]
  else
    Array.init width (fun col ->
        let idx = col * (n - 1) / (width - 1) in
        curve.(idx))

let bounds curve =
  let n = Array.length curve in
  if n = 0 then (0.0, 0.0)
  else
    Array.fold_left
      (fun (min_pnl, max_pnl) pnl ->
        (Float.min min_pnl pnl, Float.max max_pnl pnl))
      (curve.(0), curve.(0))
      curve

let row_for_value ~min_pnl ~max_pnl ~height value =
  if max_pnl = min_pnl then height / 2
  else
    let normalized = (value -. min_pnl) /. (max_pnl -. min_pnl) in
    let row =
      height - 1 - int_of_float (normalized *. float_of_int (height - 1))
    in
    clamp row 0 (height - 1)

let draw_segment grid ~x0 ~y0 ~x1 ~y1 =
  let dx = x1 - x0 in
  if dx = 0 then grid.(y1).(x1) <- '*'
  else
    for offset = 0 to dx do
      let col = x0 + offset in
      let row = y0 + ((y1 - y0) * offset / dx) in
      grid.(row).(col) <- '*'
    done

let render_ascii ?(width = 60) ?(height = 15) curve =
  let sampled = sampled_curve curve ~width in
  if Array.length sampled = 0 then "[empty pnl curve]"
  else
    let width = max 2 (Array.length sampled) in
    let height = max 4 height in
    let min_pnl, max_pnl = bounds sampled in
    let min_bound, max_bound =
      if min_pnl = max_pnl then (min_pnl -. 1.0, max_pnl +. 1.0)
      else (min_pnl, max_pnl)
    in
    let grid = Array.make_matrix height width ' ' in
    if min_bound <= 0.0 && 0.0 <= max_bound then begin
      let zero_row =
        row_for_value ~min_pnl:min_bound ~max_pnl:max_bound ~height 0.0
      in
      for col = 0 to width - 1 do
        grid.(zero_row).(col) <- '-'
      done
    end;
    let first_row =
      row_for_value ~min_pnl:min_bound ~max_pnl:max_bound ~height sampled.(0)
    in
    grid.(first_row).(0) <- '*';
    for col = 1 to width - 1 do
      let prev_row =
        row_for_value ~min_pnl:min_bound ~max_pnl:max_bound ~height
          sampled.(col - 1)
      in
      let row =
        row_for_value ~min_pnl:min_bound ~max_pnl:max_bound ~height
          sampled.(col)
      in
      draw_segment grid ~x0:(col - 1) ~y0:prev_row ~x1:col ~y1:row
    done;
    let lines =
      Array.to_list
        (Array.map
           (fun chars ->
             Printf.sprintf "|%s|" (String.init width (Array.get chars)))
           grid)
    in
    String.concat "\n"
      ([
         Printf.sprintf "PnL range: %.2f .. %.2f" min_pnl max_pnl;
         Printf.sprintf "Steps: 0 .. %d" (Array.length curve - 1);
       ]
      @ lines)

let print_ascii ?width ?height curve =
  print_endline (render_ascii ?width ?height curve)
