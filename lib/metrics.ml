type t = {
  total_pnl : float;
  sharpe_ratio : float;
  max_drawdown : float;
  win_rate : float;
  num_trades : int;
  final_pnl : float;
}

(* per-step differences of PnL curve *)
let deltas curve =
  let n = Array.length curve in
  if n <= 1 then [||]
  else Array.init (n - 1) (fun i -> curve.(i + 1) -. curve.(i))

let mean xs =
  let n = Array.length xs in
  if n = 0 then 0.0 else Array.fold_left ( +. ) 0.0 xs /. float_of_int n

let stddev xs =
  let n = Array.length xs in
  if n = 0 then 0.0
  else
    let m = mean xs in
    let sq = Array.fold_left (fun acc x -> acc +. ((x -. m) ** 2.0)) 0.0 xs in
    sqrt (sq /. float_of_int n)

let sharpe curve =
  let d = deltas curve in
  let s = stddev d in
  if s = 0.0 then 0.0 else mean d /. s

let max_drawdown curve =
  let n = Array.length curve in
  if n = 0 then 0.0
  else
    let peak = ref curve.(0) in
    let dd = ref 0.0 in
    for i = 0 to n - 1 do
      if curve.(i) > !peak then peak := curve.(i);
      let drop = !peak -. curve.(i) in
      if drop > !dd then dd := drop
    done;
    !dd

let compute ~pnl_curve ~num_trades ~winning_trades =
  let n = Array.length pnl_curve in
  let final = if n = 0 then 0.0 else pnl_curve.(n - 1) in
  let win_rate =
    if num_trades = 0 then 0.0
    else float_of_int winning_trades /. float_of_int num_trades
  in
  {
    total_pnl = final;
    sharpe_ratio = sharpe pnl_curve;
    max_drawdown = max_drawdown pnl_curve;
    win_rate;
    num_trades;
    final_pnl = final;
  }

let to_csv m path =
  let oc = open_out path in
  output_string oc
    "total_pnl,sharpe_ratio,max_drawdown,win_rate,num_trades,final_pnl\n";
  Printf.fprintf oc "%f,%f,%f,%f,%d,%f\n" m.total_pnl m.sharpe_ratio
    m.max_drawdown m.win_rate m.num_trades m.final_pnl;
  close_out oc

let print m =
  Printf.printf "Total PnL:     $%.2f\n" m.total_pnl;
  Printf.printf "Sharpe ratio:  %.4f\n" m.sharpe_ratio;
  Printf.printf "Max drawdown:  $%.2f\n" m.max_drawdown;
  Printf.printf "Win rate:      %.2f%% (%d / %d trades)\n"
    (m.win_rate *. 100.0)
    (int_of_float ((m.win_rate *. float_of_int m.num_trades) +. 0.5))
    m.num_trades;
  Printf.printf "Final PnL:     $%.2f\n" m.final_pnl
