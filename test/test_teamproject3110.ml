open Teamproject3110

let approx_eq a b = Float.abs (a -. b) < 1e-6

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    exit 1)

let test_flat_curve () =
  let m =
    Metrics.compute ~pnl_curve:[| 0.0; 0.0; 0.0 |] ~num_trades:0
      ~winning_trades:0
  in
  check "flat: final_pnl = 0" (approx_eq m.final_pnl 0.0);
  check "flat: drawdown = 0" (approx_eq m.max_drawdown 0.0);
  check "flat: sharpe = 0 (no variance)" (approx_eq m.sharpe_ratio 0.0);
  check "flat: win_rate = 0 (no trades)" (approx_eq m.win_rate 0.0)

let test_monotonic_up () =
  let m =
    Metrics.compute
      ~pnl_curve:[| 0.0; 10.0; 20.0; 30.0 |]
      ~num_trades:3 ~winning_trades:3
  in
  check "up: final_pnl = 30" (approx_eq m.final_pnl 30.0);
  check "up: total_pnl = 30" (approx_eq m.total_pnl 30.0);
  check "up: drawdown = 0" (approx_eq m.max_drawdown 0.0);
  check "up: win_rate = 1.0" (approx_eq m.win_rate 1.0);
  check "up: num_trades = 3" (m.num_trades = 3)

let test_drawdown () =
  let m =
    Metrics.compute
      ~pnl_curve:[| 0.0; 50.0; 100.0; 40.0; 60.0 |]
      ~num_trades:4 ~winning_trades:2
  in
  check "dd: peak 100 -> trough 40 => 60" (approx_eq m.max_drawdown 60.0);
  check "dd: final_pnl = 60" (approx_eq m.final_pnl 60.0);
  check "dd: win_rate = 0.5" (approx_eq m.win_rate 0.5)

let test_losses () =
  let m =
    Metrics.compute ~pnl_curve:[| 0.0; -10.0; -25.0 |] ~num_trades:2
      ~winning_trades:0
  in
  check "loss: final_pnl = -25" (approx_eq m.final_pnl (-25.0));
  check "loss: drawdown = 25 (peak was 0)" (approx_eq m.max_drawdown 25.0);
  check "loss: win_rate = 0" (approx_eq m.win_rate 0.0)

let test_sharpe_positive () =
  let m =
    Metrics.compute ~pnl_curve:[| 0.0; 1.0; 2.0; 3.0 |] ~num_trades:3
      ~winning_trades:3
  in
  check "sharpe: constant positive deltas => 0 (zero std)"
    (approx_eq m.sharpe_ratio 0.0)

let test_config_state_count () =
  let expected =
    Config.num_return_buckets * Config.num_trend_buckets
    * Config.num_volatility_buckets * Config.num_position_buckets
  in
  check "config: num_states matches product" (Config.num_states = expected);
  check "config: no shorting (position buckets = max_position + 1)"
    (Config.num_position_buckets = Config.max_position + 1)

let () =
  Printf.printf "Running Metrics tests...\n";
  test_flat_curve ();
  test_monotonic_up ();
  test_drawdown ();
  test_losses ();
  test_sharpe_positive ();
  test_config_state_count ();
  Printf.printf "All tests passed.\n"
