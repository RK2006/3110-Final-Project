open OUnit2
open Teamproject3110

let eps = 1e-6

let assert_float_eq ?(tol = eps) ~msg expected actual =
  assert_bool
    (Printf.sprintf "%s: expected %f, got %f" msg expected actual)
    (Float.abs (expected -. actual) < tol)

let string_of_action = function
  | Environment.Sell -> "Sell"
  | Environment.Hold -> "Hold"
  | Environment.Buy -> "Buy"

(* functorized test Modules *)

module MetricsTester (M : module type of Metrics) = struct
  let test_metrics_flat _ =
    let m =
      M.compute ~pnl_curve:[| 0.0; 0.0; 0.0 |] ~num_trades:0 ~winning_trades:0
    in
    assert_float_eq ~msg:"final_pnl" 0.0 m.final_pnl;
    assert_float_eq ~msg:"max_drawdown" 0.0 m.max_drawdown;
    assert_float_eq ~msg:"sharpe_ratio" 0.0 m.sharpe_ratio;
    assert_float_eq ~msg:"win_rate" 0.0 m.win_rate

  let test_metrics_monotonic_up _ =
    let m =
      M.compute
        ~pnl_curve:[| 0.0; 10.0; 20.0; 30.0 |]
        ~num_trades:3 ~winning_trades:3
    in
    assert_float_eq ~msg:"final_pnl" 30.0 m.final_pnl;
    assert_float_eq ~msg:"total_pnl" 30.0 m.total_pnl;
    assert_float_eq ~msg:"max_drawdown" 0.0 m.max_drawdown;
    assert_float_eq ~msg:"win_rate" 1.0 m.win_rate;
    assert_equal ~msg:"num_trades" ~printer:string_of_int 3 m.num_trades

  let test_metrics_drawdown _ =
    let m =
      M.compute
        ~pnl_curve:[| 0.0; 50.0; 100.0; 40.0; 60.0 |]
        ~num_trades:4 ~winning_trades:2
    in
    assert_float_eq ~msg:"max_drawdown" 60.0 m.max_drawdown;
    assert_float_eq ~msg:"final_pnl" 60.0 m.final_pnl;
    assert_float_eq ~msg:"win_rate" 0.5 m.win_rate

  let test_metrics_losses _ =
    let m =
      M.compute ~pnl_curve:[| 0.0; -10.0; -25.0 |] ~num_trades:2
        ~winning_trades:0
    in
    assert_float_eq ~msg:"final_pnl" (-25.0) m.final_pnl;
    assert_float_eq ~msg:"max_drawdown" 25.0 m.max_drawdown;
    assert_float_eq ~msg:"win_rate" 0.0 m.win_rate

  let test_metrics_sharpe_zero_variance _ =
    let m =
      M.compute ~pnl_curve:[| 0.0; 1.0; 2.0; 3.0 |] ~num_trades:3
        ~winning_trades:3
    in
    assert_float_eq ~msg:"sharpe with zero variance" 0.0 m.sharpe_ratio

  let tests =
    [
      "metrics_flat" >:: test_metrics_flat;
      "metrics_monotonic_up" >:: test_metrics_monotonic_up;
      "metrics_drawdown" >:: test_metrics_drawdown;
      "metrics_losses" >:: test_metrics_losses;
      "metrics_sharpe_zero_variance" >:: test_metrics_sharpe_zero_variance;
    ]
end

module ConfigTester (C : module type of Config) = struct
  let test_config_state_count _ =
    let expected =
      C.num_return_buckets * C.num_trend_buckets * C.num_volatility_buckets
      * C.num_position_buckets
    in
    assert_equal ~msg:"num_states matches product" ~printer:string_of_int
      expected C.num_states

  let test_config_no_shorting _ =
    assert_equal ~msg:"position buckets = max_position + 1"
      ~printer:string_of_int (C.max_position + 1) C.num_position_buckets

  let tests =
    [
      "config_state_count" >:: test_config_state_count;
      "config_no_shorting" >:: test_config_no_shorting;
    ]
end

module AgentTester (A : module type of Agent) = struct
  let test_agent_create_epsilon _ =
    let a =
      A.create ~num_states:10 ~num_actions:3 ~alpha:0.1 ~gamma:0.9 ~epsilon:0.3
    in
    assert_float_eq ~msg:"epsilon stored" 0.3 (A.epsilon a)

  let test_agent_greedy_picks_max _ =
    let a =
      A.create ~num_states:4 ~num_actions:3 ~alpha:1.0 ~gamma:0.0 ~epsilon:0.0
    in
    A.update a ~state:0 ~action:2 ~reward:5.0 ~next_state:1 ~terminal:true;
    assert_equal ~msg:"greedy picks best action" ~printer:string_of_int 2
      (A.act a ~state:0 ~training:false)

  let test_agent_training_epsilon_zero _ =
    let a =
      A.create ~num_states:4 ~num_actions:3 ~alpha:1.0 ~gamma:0.0 ~epsilon:0.0
    in
    A.update a ~state:0 ~action:1 ~reward:2.0 ~next_state:0 ~terminal:true;
    assert_equal ~msg:"epsilon=0 is greedy even in training"
      ~printer:string_of_int 1 (A.act a ~state:0 ~training:true)

  let test_agent_update_terminal _ =
    let a =
      A.create ~num_states:2 ~num_actions:2 ~alpha:0.5 ~gamma:0.9 ~epsilon:0.0
    in
    A.update a ~state:0 ~action:0 ~reward:10.0 ~next_state:1 ~terminal:true;
    assert_equal ~msg:"updated action becomes greedy" ~printer:string_of_int 0
      (A.act a ~state:0 ~training:false)

  let test_agent_update_nonterminal_bootstrap _ =
    let a =
      A.create ~num_states:2 ~num_actions:2 ~alpha:1.0 ~gamma:0.5 ~epsilon:0.0
    in
    A.update a ~state:1 ~action:1 ~reward:4.0 ~next_state:0 ~terminal:true;
    A.update a ~state:0 ~action:0 ~reward:1.0 ~next_state:1 ~terminal:false;
    assert_equal ~msg:"bootstrap update makes action 0 greedy"
      ~printer:string_of_int 0 (A.act a ~state:0 ~training:false)

  let test_agent_decay_epsilon _ =
    let a =
      A.create ~num_states:1 ~num_actions:2 ~alpha:0.1 ~gamma:0.9 ~epsilon:1.0
    in
    A.decay_epsilon a ~rate:0.1 ~min_epsilon:0.05;
    assert_float_eq ~msg:"decayed once" 0.9 (A.epsilon a);
    A.decay_epsilon a ~rate:0.99 ~min_epsilon:0.05;
    assert_float_eq ~msg:"clamps to min_epsilon" 0.05 (A.epsilon a)

  let test_agent_save_load_roundtrip _ =
    let a =
      A.create ~num_states:3 ~num_actions:2 ~alpha:0.2 ~gamma:0.8 ~epsilon:0.4
    in
    A.update a ~state:1 ~action:0 ~reward:7.0 ~next_state:2 ~terminal:true;
    A.update a ~state:2 ~action:1 ~reward:(-1.5) ~next_state:0 ~terminal:true;
    let path = Filename.temp_file "qtable" ".csv" in
    A.save a path;
    let b = A.load path in
    Sys.remove path;
    assert_float_eq ~msg:"epsilon preserved" (A.epsilon a) (A.epsilon b);
    assert_equal ~msg:"greedy action preserved at state 1"
      ~printer:string_of_int
      (A.act a ~state:1 ~training:false)
      (A.act b ~state:1 ~training:false);
    assert_equal ~msg:"greedy action preserved at state 2"
      ~printer:string_of_int
      (A.act a ~state:2 ~training:false)
      (A.act b ~state:2 ~training:false)

  let tests =
    [
      "agent_create_epsilon" >:: test_agent_create_epsilon;
      "agent_greedy_picks_max" >:: test_agent_greedy_picks_max;
      "agent_training_epsilon_zero" >:: test_agent_training_epsilon_zero;
      "agent_update_terminal" >:: test_agent_update_terminal;
      "agent_update_nonterminal_bootstrap"
      >:: test_agent_update_nonterminal_bootstrap;
      "agent_decay_epsilon" >:: test_agent_decay_epsilon;
      "agent_save_load_roundtrip" >:: test_agent_save_load_roundtrip;
    ]
end

(* abstracting the Environment tests needs both Environment and Config logic *)
module EnvironmentTester
    (E : module type of Environment)
    (C : module type of Config) =
struct
  open E

  let write_price_csv prices =
    let path = Filename.temp_file "prices" ".csv" in
    let oc = open_out path in
    output_string oc "Date,Close\n";
    Array.iteri
      (fun i p -> Printf.fprintf oc "2020-01-%02d,%f\n" ((i mod 28) + 1) p)
      prices;
    close_out oc;
    path

  let sample_prices n =
    Array.init n (fun i ->
        100.0 +. float_of_int i +. if i mod 2 = 0 then 0.5 else -0.5)

  let warmup () = max C.long_ma_window C.volatility_window

  let test_env_action_int_roundtrip _ =
    List.iter
      (fun a ->
        assert_equal ~msg:"action_of_int . int_of_action = id"
          ~printer:string_of_action a
          (action_of_int (int_of_action a)))
      [ Sell; Hold; Buy ];
    assert_equal ~msg:"Sell = 0" ~printer:string_of_int 0 (int_of_action Sell);
    assert_equal ~msg:"Hold = 1" ~printer:string_of_int 1 (int_of_action Hold);
    assert_equal ~msg:"Buy = 2" ~printer:string_of_int 2 (int_of_action Buy)

  let test_env_action_of_int_invalid _ =
    assert_raises ~msg:"negative index" (Failure "action_of_int: invalid -1")
      (fun () -> action_of_int (-1));
    assert_raises ~msg:"out-of-range index" (Failure "action_of_int: invalid 3")
      (fun () -> action_of_int 3)

  let test_env_load_too_short _ =
    let path = write_price_csv (sample_prices (warmup () + 1)) in
    let finally () = Sys.remove path in
    let did_raise =
      try
        let _ = load path in
        false
      with Failure _ -> true
    in
    finally ();
    assert_bool "load should fail for too-short series" did_raise

  let test_env_load_and_reset _ =
    let n = warmup () + 40 in
    let path = write_price_csv (sample_prices n) in
    let env = load path in
    Sys.remove path;
    let s = reset env in
    assert_bool "state id in valid range" (s >= 0 && s < C.num_states);
    assert_equal ~msg:"pnl starts at 0" ~printer:string_of_float 0.0 (pnl env);
    assert_equal ~msg:"episode_length = n - 1 - warmup" ~printer:string_of_int
      (n - 1 - warmup ())
      (episode_length env);
    assert_equal ~msg:"current_state matches reset state" ~printer:string_of_int
      s (current_state env)

  let test_env_hold_zero_position_zero_reward _ =
    let path = write_price_csv (sample_prices (warmup () + 40)) in
    let env = load path in
    Sys.remove path;
    let _ = reset env in
    let r = step env Hold in
    assert_float_eq ~msg:"hold at pos 0 yields zero reward" 0.0 r.reward;
    assert_float_eq ~msg:"pnl unchanged" 0.0 (pnl env)

  let test_env_sell_at_zero_is_noop _ =
    let path = write_price_csv (sample_prices (warmup () + 40)) in
    let env = load path in
    Sys.remove path;
    let _ = reset env in
    let r = step env Sell in
    assert_float_eq ~msg:"sell at pos 0 is costless" 0.0 r.reward

  let test_env_buy_incurs_cost _ =
    let path = write_price_csv (sample_prices (warmup () + 40)) in
    let env = load path in
    Sys.remove path;
    let _ = reset env in
    let before = pnl env in
    let r = step env Buy in
    assert_float_eq ~msg:"pnl delta equals step reward" r.reward
      (pnl env -. before)

  let test_env_terminal_at_end _ =
    let path = write_price_csv (sample_prices (warmup () + 10)) in
    let env = load path in
    Sys.remove path;
    let _ = reset env in
    let len = episode_length env in
    let last_terminal = ref false in
    for _ = 1 to len do
      let r = step env Hold in
      last_terminal := r.terminal
    done;
    assert_bool "final step is terminal" !last_terminal

  let test_env_state_in_range_after_steps _ =
    let path = write_price_csv (sample_prices (warmup () + 30)) in
    let env = load path in
    Sys.remove path;
    let _ = reset env in
    for _ = 1 to 10 do
      let r = step env Buy in
      assert_bool "next_state in range"
        (r.next_state >= 0 && r.next_state < C.num_states)
    done

  let tests =
    [
      "env_action_int_roundtrip" >:: test_env_action_int_roundtrip;
      "env_action_of_int_invalid" >:: test_env_action_of_int_invalid;
      "env_load_too_short" >:: test_env_load_too_short;
      "env_load_and_reset" >:: test_env_load_and_reset;
      "env_hold_zero_position_zero_reward"
      >:: test_env_hold_zero_position_zero_reward;
      "env_sell_at_zero_is_noop" >:: test_env_sell_at_zero_is_noop;
      "env_buy_incurs_cost" >:: test_env_buy_incurs_cost;
      "env_terminal_at_end" >:: test_env_terminal_at_end;
      "env_state_in_range_after_steps" >:: test_env_state_in_range_after_steps;
    ]
end

module SvgPlotTester (S : module type of Svg_plot) = struct
  let read_all path =
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s

  let substring_count ~needle haystack =
    if needle = "" then 0
    else
      let nl = String.length needle in
      let hl = String.length haystack in
      let count = ref 0 in
      let i = ref 0 in
      while !i <= hl - nl do
        if String.sub haystack !i nl = needle then begin
          incr count;
          i := !i + nl
        end
        else incr i
      done;
      !count

  let contains haystack needle = substring_count ~needle haystack > 0

  let test_render_basic_structure _ =
    let svg = S.render [| 0.0; 1.0; -1.0; 2.0 |] in
    assert_bool "starts with <svg" (contains svg "<svg ");
    assert_bool "has style block" (contains svg "<style>");
    assert_bool "has frame rect" (contains svg "class=\"frame\"");
    assert_bool "has pnl polyline" (contains svg "class=\"pnl\"");
    assert_bool "has zero line when crossing zero" (contains svg "class=\"zero\"");
    assert_bool "has closing tag" (contains svg "</svg>")

  let test_render_no_zero_line_when_strictly_positive _ =
    let svg = S.render [| 1.0; 2.0; 3.0 |] in
    assert_bool "no zero line for strictly positive curve"
      (not (contains svg "class=\"zero\""))

  let test_render_with_actions_includes_markers _ =
    let actions =
      [| Environment.Buy; Environment.Hold; Environment.Sell; Environment.Hold |]
    in
    let curve = [| 0.0; 1.0; 1.5; 0.5; 0.8 |] in
    let svg = S.render ~actions curve in
    assert_equal
      ~msg:"buy class occurs twice (marker + legend)"
      ~printer:string_of_int 2
      (substring_count ~needle:"class=\"buy\"" svg);
    assert_equal
      ~msg:"sell class occurs twice (marker + legend)"
      ~printer:string_of_int 2
      (substring_count ~needle:"class=\"sell\"" svg)

  let test_render_no_legend_markers_without_actions _ =
    let svg = S.render [| 0.0; 1.0; 2.0 |] in
    assert_equal ~msg:"no buy class without actions" 0
      (substring_count ~needle:"class=\"buy\"" svg) ~printer:string_of_int;
    assert_equal ~msg:"no sell class without actions" 0
      (substring_count ~needle:"class=\"sell\"" svg) ~printer:string_of_int

  let test_render_custom_title _ =
    let config = { S.default_config with title = "My Run" } in
    let svg = S.render ~config [| 0.0; 1.0 |] in
    assert_bool "title appears" (contains svg ">My Run<")

  let test_render_escapes_xml_in_title _ =
    let config = { S.default_config with title = "A & B <c>" } in
    let svg = S.render ~config [| 0.0; 1.0 |] in
    assert_bool "ampersand escaped" (contains svg "A &amp; B");
    assert_bool "lt escaped" (contains svg "&lt;c&gt;");
    assert_bool "no raw lt in title region"
      (not (contains svg ">A & B <c><"))

  let test_render_flat_curve _ =
    let svg = S.render [| 5.0; 5.0; 5.0 |] in
    assert_bool "flat curve still produces a polyline"
      (contains svg "class=\"pnl\"")

  let test_write_round_trip _ =
    let path = Filename.temp_file "svg" ".svg" in
    S.write ~path [| 0.0; 2.0; -1.0 |];
    let contents = read_all path in
    Sys.remove path;
    assert_bool "file starts with <svg"
      (String.length contents > 5 && String.sub contents 0 5 = "<svg ")

  let tests =
    [
      "svg_render_basic_structure" >:: test_render_basic_structure;
      "svg_render_no_zero_line_when_strictly_positive"
      >:: test_render_no_zero_line_when_strictly_positive;
      "svg_render_with_actions_includes_markers"
      >:: test_render_with_actions_includes_markers;
      "svg_render_no_legend_markers_without_actions"
      >:: test_render_no_legend_markers_without_actions;
      "svg_render_custom_title" >:: test_render_custom_title;
      "svg_render_escapes_xml_in_title" >:: test_render_escapes_xml_in_title;
      "svg_render_flat_curve" >:: test_render_flat_curve;
      "svg_write_round_trip" >:: test_write_round_trip;
    ]
end

module TradeStatsTester (T : module type of Trade_stats) = struct
  let test_create_zero _ =
    let t = T.create () in
    let s = T.summary t in
    assert_equal ~msg:"create: num_closed_trades" ~printer:string_of_int 0
      s.num_closed_trades;
    assert_equal ~msg:"create: winning_closed_trades" ~printer:string_of_int 0
      s.winning_closed_trades;
    assert_float_eq ~msg:"create: realized_pnl" 0.0 s.realized_pnl

  let test_record_zero_is_noop _ =
    let t = T.create () in
    T.record_fill t ~delta_position:0 ~price:100.0 ~transaction_cost:0.001;
    let s = T.summary t in
    assert_equal ~msg:"no trades recorded" ~printer:string_of_int 0
      s.num_closed_trades

  let test_buy_then_sell_profit _ =
    let t = T.create () in
    T.record_fill t ~delta_position:1 ~price:100.0 ~transaction_cost:0.0;
    T.record_fill t ~delta_position:(-1) ~price:110.0 ~transaction_cost:0.0;
    let s = T.summary t in
    assert_equal ~msg:"one closed trade" ~printer:string_of_int 1
      s.num_closed_trades;
    assert_equal ~msg:"one winning trade" ~printer:string_of_int 1
      s.winning_closed_trades;
    assert_float_eq ~msg:"realized 10.0 pnl" 10.0 s.realized_pnl

  let test_buy_then_sell_loss _ =
    let t = T.create () in
    T.record_fill t ~delta_position:1 ~price:100.0 ~transaction_cost:0.0;
    T.record_fill t ~delta_position:(-1) ~price:90.0 ~transaction_cost:0.0;
    let s = T.summary t in
    assert_equal ~msg:"one closed trade" ~printer:string_of_int 1
      s.num_closed_trades;
    assert_equal ~msg:"no winners" ~printer:string_of_int 0
      s.winning_closed_trades;
    assert_float_eq ~msg:"realized -10.0 pnl" (-10.0) s.realized_pnl

  let test_transaction_cost_applied _ =
    let t = T.create () in
    T.record_fill t ~delta_position:1 ~price:100.0 ~transaction_cost:0.01;
    T.record_fill t ~delta_position:(-1) ~price:100.0 ~transaction_cost:0.01;
    let s = T.summary t in
    assert_float_eq ~msg:"two-sided cost makes round-trip a loss" (-2.0)
      s.realized_pnl;
    assert_equal ~msg:"no winners with costs only" ~printer:string_of_int 0
      s.winning_closed_trades

  let test_fifo_lot_matching _ =
    let t = T.create () in
    T.record_fill t ~delta_position:1 ~price:50.0 ~transaction_cost:0.0;
    T.record_fill t ~delta_position:1 ~price:100.0 ~transaction_cost:0.0;
    T.record_fill t ~delta_position:(-1) ~price:60.0 ~transaction_cost:0.0;
    let s = T.summary t in
    assert_equal ~msg:"one closed trade so far" ~printer:string_of_int 1
      s.num_closed_trades;
    assert_float_eq ~msg:"FIFO: first lot at 50 closed at 60" 10.0
      s.realized_pnl;
    T.record_fill t ~delta_position:(-1) ~price:90.0 ~transaction_cost:0.0;
    let s2 = T.summary t in
    assert_equal ~msg:"two closed trades" ~printer:string_of_int 2
      s2.num_closed_trades;
    assert_float_eq ~msg:"FIFO: 10 + (90-100) = 0" 0.0 s2.realized_pnl

  let test_record_fill_invalid_price _ =
    let t = T.create () in
    assert_raises ~msg:"price 0 rejected"
      (Invalid_argument "record_fill: price must be positive") (fun () ->
        T.record_fill t ~delta_position:1 ~price:0.0 ~transaction_cost:0.0);
    assert_raises ~msg:"negative price rejected"
      (Invalid_argument "record_fill: price must be positive") (fun () ->
        T.record_fill t ~delta_position:1 ~price:(-1.0) ~transaction_cost:0.0)

  let test_record_fill_invalid_cost _ =
    let t = T.create () in
    assert_raises ~msg:"negative cost rejected"
      (Invalid_argument "record_fill: transaction_cost must be in [0, 1]")
      (fun () ->
        T.record_fill t ~delta_position:1 ~price:10.0
          ~transaction_cost:(-0.01));
    assert_raises ~msg:"cost > 1 rejected"
      (Invalid_argument "record_fill: transaction_cost must be in [0, 1]")
      (fun () ->
        T.record_fill t ~delta_position:1 ~price:10.0 ~transaction_cost:1.5)

  let test_record_fill_invalid_delta _ =
    let t = T.create () in
    assert_raises ~msg:"delta=2 rejected"
      (Invalid_argument "record_fill: delta_position must be -1, 0, or 1")
      (fun () ->
        T.record_fill t ~delta_position:2 ~price:10.0 ~transaction_cost:0.0)

  let test_sell_without_lot_fails _ =
    let t = T.create () in
    assert_raises ~msg:"sell with no lots rejected"
      (Invalid_argument "record_fill: sell exceeds open lots") (fun () ->
        T.record_fill t ~delta_position:(-1) ~price:10.0 ~transaction_cost:0.0)

  let test_reset_clears_state _ =
    let t = T.create () in
    T.record_fill t ~delta_position:1 ~price:10.0 ~transaction_cost:0.0;
    T.record_fill t ~delta_position:(-1) ~price:20.0 ~transaction_cost:0.0;
    T.reset t;
    let s = T.summary t in
    assert_equal ~msg:"trades cleared" ~printer:string_of_int 0
      s.num_closed_trades;
    assert_float_eq ~msg:"pnl cleared" 0.0 s.realized_pnl;
    assert_raises ~msg:"open lots cleared too"
      (Invalid_argument "record_fill: sell exceeds open lots") (fun () ->
        T.record_fill t ~delta_position:(-1) ~price:10.0 ~transaction_cost:0.0)

  let tests =
    [
      "trade_stats_create_zero" >:: test_create_zero;
      "trade_stats_record_zero_is_noop" >:: test_record_zero_is_noop;
      "trade_stats_buy_then_sell_profit" >:: test_buy_then_sell_profit;
      "trade_stats_buy_then_sell_loss" >:: test_buy_then_sell_loss;
      "trade_stats_transaction_cost_applied" >:: test_transaction_cost_applied;
      "trade_stats_fifo_lot_matching" >:: test_fifo_lot_matching;
      "trade_stats_record_fill_invalid_price"
      >:: test_record_fill_invalid_price;
      "trade_stats_record_fill_invalid_cost" >:: test_record_fill_invalid_cost;
      "trade_stats_record_fill_invalid_delta"
      >:: test_record_fill_invalid_delta;
      "trade_stats_sell_without_lot_fails" >:: test_sell_without_lot_fails;
      "trade_stats_reset_clears_state" >:: test_reset_clears_state;
    ]
end

(* Shared helper that writes a CSV price series and returns the path. *)
let write_price_csv prices =
  let path = Filename.temp_file "prices" ".csv" in
  let oc = open_out path in
  output_string oc "Date,Close\n";
  Array.iteri
    (fun i p -> Printf.fprintf oc "2020-01-%02d,%f\n" ((i mod 28) + 1) p)
    prices;
  close_out oc;
  path

let trending_prices n =
  Array.init n (fun i ->
      100.0 +. (0.5 *. float_of_int i)
      +. if i mod 3 = 0 then 0.25 else -0.25)

let env_warmup () = max Config.long_ma_window Config.volatility_window

let make_env n =
  let path = write_price_csv (trending_prices n) in
  let env = Environment.load path in
  Sys.remove path;
  env

module BaselinesTester (B : module type of Baselines) = struct
  let test_buy_and_hold_curve_length _ =
    let n = env_warmup () + 60 in
    let env = make_env n in
    let r = B.buy_and_hold env in
    assert_equal ~msg:"pnl_curve length = episode_length + 1"
      ~printer:string_of_int
      (Environment.episode_length env + 1)
      (Array.length r.pnl_curve);
    assert_float_eq ~msg:"pnl_curve starts at 0" 0.0 r.pnl_curve.(0)

  let test_buy_and_hold_no_closed_trades _ =
    let env = make_env (env_warmup () + 60) in
    let r = B.buy_and_hold env in
    assert_equal ~msg:"buy-and-hold never sells, so 0 closed trades"
      ~printer:string_of_int 0 r.num_trades;
    assert_equal ~msg:"and 0 wins" ~printer:string_of_int 0 r.winning_trades

  let test_random_policy_deterministic _ =
    let env_a = make_env (env_warmup () + 60) in
    let env_b = make_env (env_warmup () + 60) in
    let r1 = B.random_policy ~seed:7 env_a in
    let r2 = B.random_policy ~seed:7 env_b in
    assert_equal ~msg:"same seed -> same final pnl"
      ~printer:string_of_float
      r1.pnl_curve.(Array.length r1.pnl_curve - 1)
      r2.pnl_curve.(Array.length r2.pnl_curve - 1)

  let test_random_policy_curve_length _ =
    let env = make_env (env_warmup () + 50) in
    let r = B.random_policy ~seed:1 env in
    assert_equal ~msg:"pnl_curve length = episode_length + 1"
      ~printer:string_of_int
      (Environment.episode_length env + 1)
      (Array.length r.pnl_curve)

  let test_sma_crossover_runs _ =
    let env = make_env (env_warmup () + 80) in
    let r = B.sma_crossover env in
    assert_bool "sma_crossover produces non-empty curve"
      (Array.length r.pnl_curve > 1);
    assert_bool "sma_crossover never goes below zero trades"
      (r.num_trades >= 0);
    assert_bool "winning_trades <= num_trades"
      (r.winning_trades <= r.num_trades)

  let tests =
    [
      "baselines_buy_and_hold_curve_length" >:: test_buy_and_hold_curve_length;
      "baselines_buy_and_hold_no_closed_trades"
      >:: test_buy_and_hold_no_closed_trades;
      "baselines_random_policy_deterministic"
      >:: test_random_policy_deterministic;
      "baselines_random_policy_curve_length"
      >:: test_random_policy_curve_length;
      "baselines_sma_crossover_runs" >:: test_sma_crossover_runs;
    ]
end

module PnlPlotTester (P : module type of Pnl_plot) = struct
  let read_all path =
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s

  let count_lines s =
    let count = ref 0 in
    String.iter (fun c -> if c = '\n' then incr count) s;
    !count

  let test_write_csv_header_and_rows _ =
    let curve = [| 0.0; 1.5; -2.0; 3.25 |] in
    let path = Filename.temp_file "pnl" ".csv" in
    P.write_csv curve path;
    let contents = read_all path in
    Sys.remove path;
    assert_bool "starts with header"
      (String.length contents >= 9 && String.sub contents 0 9 = "step,pnl\n");
    assert_equal ~msg:"row count matches curve" ~printer:string_of_int
      (Array.length curve + 1)
      (count_lines contents)

  let test_render_ascii_empty _ =
    let s = P.render_ascii [||] in
    assert_equal ~msg:"empty curve returns sentinel" ~printer:(fun s -> s)
      "[empty pnl curve]" s

  let test_render_ascii_two_points _ =
    let s = P.render_ascii [| 0.0; 1.0 |] in
    assert_bool "two-point curve renders" (String.length s > 0);
    assert_bool "two-point curve has marker" (String.contains s '*')

  let test_render_ascii_flat _ =
    let s = P.render_ascii [| 5.0; 5.0; 5.0; 5.0 |] in
    assert_bool "flat curve still renders header line"
      (String.length s > 0
      && String.length s
         >= String.length "PnL range: 5.00 .. 5.00"
      && String.sub s 0 22 = "PnL range: 5.00 .. 5.0")

  let test_render_ascii_varying_has_marker _ =
    let s = P.render_ascii [| 0.0; 1.0; 2.0; 3.0; 2.0; 1.0; 0.0 |] in
    assert_bool "rendered curve contains a path marker"
      (String.contains s '*')

  let test_render_ascii_zero_line_when_crossing _ =
    let s = P.render_ascii [| -1.0; 0.5; -0.5; 1.0 |] in
    assert_bool "zero line drawn when range straddles zero"
      (String.contains s '-')

  let tests =
    [
      "pnl_plot_write_csv_header_and_rows" >:: test_write_csv_header_and_rows;
      "pnl_plot_render_ascii_empty" >:: test_render_ascii_empty;
      "pnl_plot_render_ascii_two_points" >:: test_render_ascii_two_points;
      "pnl_plot_render_ascii_flat" >:: test_render_ascii_flat;
      "pnl_plot_render_ascii_varying_has_marker"
      >:: test_render_ascii_varying_has_marker;
      "pnl_plot_render_ascii_zero_line_when_crossing"
      >:: test_render_ascii_zero_line_when_crossing;
    ]
end

module ExperimentTester (X : module type of Experiment) = struct
  let test_default_hyperparameters_q_learning _ =
    let h = X.default_hyperparameters in
    assert_bool "default algorithm is Q_learning"
      (match h.algorithm with X.Q_learning -> true | _ -> false);
    assert_bool "alpha in (0, 1]" (h.alpha > 0.0 && h.alpha <= 1.0);
    assert_bool "gamma in (0, 1]" (h.gamma > 0.0 && h.gamma <= 1.0);
    assert_bool "epsilon_min <= epsilon_start"
      (h.epsilon_min <= h.epsilon_start);
    assert_bool "num_episodes positive" (h.num_episodes > 0)

  let test_describe_hyperparameters_format _ =
    let h = X.default_hyperparameters in
    let s = X.describe_hyperparameters h in
    let contains needle haystack =
      let nl = String.length needle in
      let hl = String.length haystack in
      let found = ref false in
      let i = ref 0 in
      while (not !found) && !i <= hl - nl do
        if String.sub haystack !i nl = needle then found := true;
        incr i
      done;
      !found
    in
    assert_bool "mentions algorithm" (contains "algorithm=" s);
    assert_bool "mentions alpha" (contains "alpha=" s);
    assert_bool "mentions seed" (contains "seed=" s);
    assert_bool "Q_learning maps to q_learning"
      (contains "algorithm=q_learning" s)

  let test_expand_grid_cardinality _ =
    let g =
      {
        X.alphas = [ 0.1; 0.2 ];
        gammas = [ 0.9 ];
        epsilon_decays = [ 0.001; 0.01; 0.1 ];
        num_episodes_values = [ 100; 200 ];
      }
    in
    let runs = X.expand_grid ~base:X.default_hyperparameters g in
    assert_equal ~msg:"|alphas| * |gammas| * |decays| * |episodes|"
      ~printer:string_of_int (2 * 1 * 3 * 2) (List.length runs)

  let test_expand_grid_preserves_base_seed _ =
    let base = { X.default_hyperparameters with seed = 17 } in
    let g =
      {
        X.alphas = [ 0.05 ];
        gammas = [ 0.9 ];
        epsilon_decays = [ 0.01 ];
        num_episodes_values = [ 50 ];
      }
    in
    let runs = X.expand_grid ~base g in
    match runs with
    | [ h ] ->
        assert_equal ~msg:"seed inherited from base" ~printer:string_of_int 17
          h.seed;
        assert_equal ~msg:"alpha taken from grid"
          ~printer:string_of_float 0.05 h.alpha;
        assert_equal ~msg:"num_episodes taken from grid"
          ~printer:string_of_int 50 h.num_episodes
    | _ -> assert_failure "expected exactly one expanded run"

  let test_best_run_empty _ =
    assert_equal ~msg:"empty list -> None"
      ~printer:(function None -> "None" | Some _ -> "Some _")
      None (X.best_run [])

  let make_run total_pnl =
    let pnl_curve =
      [|
        0.0;
        total_pnl /. 2.0;
        total_pnl;
      |]
    in
    let trade_summary =
      Trade_stats.{ num_closed_trades = 0; winning_closed_trades = 0;
                    realized_pnl = 0.0 }
    in
    let metrics =
      Metrics.compute ~pnl_curve ~num_trades:0 ~winning_trades:0
    in
    {
      X.hyperparameters = X.default_hyperparameters;
      agent =
        Agent.create ~num_states:1 ~num_actions:1 ~alpha:0.1 ~gamma:0.9
          ~epsilon:0.0;
      evaluation =
        { pnl_curve; metrics; trade_summary; actions = [||] };
    }

  let test_best_run_picks_max_pnl _ =
    let runs = [ make_run 10.0; make_run 25.0; make_run 5.0 ] in
    match X.best_run runs with
    | None -> assert_failure "expected Some best"
    | Some r ->
        assert_float_eq ~msg:"best total_pnl is 25" 25.0
          r.evaluation.metrics.total_pnl

  let test_evaluate_policy_hold_only _ =
    let env = make_env (env_warmup () + 50) in
    let policy _ = Environment.Hold in
    let ev = X.evaluate_policy env policy in
    assert_equal ~msg:"pnl_curve length = episode_length + 1"
      ~printer:string_of_int
      (Environment.episode_length env + 1)
      (Array.length ev.pnl_curve);
    assert_equal ~msg:"actions length = episode_length"
      ~printer:string_of_int
      (Environment.episode_length env)
      (Array.length ev.actions);
    assert_float_eq ~msg:"hold-only never trades, pnl stays at 0"
      0.0 ev.metrics.total_pnl

  let tests =
    [
      "experiment_default_hyperparameters_q_learning"
      >:: test_default_hyperparameters_q_learning;
      "experiment_describe_hyperparameters_format"
      >:: test_describe_hyperparameters_format;
      "experiment_expand_grid_cardinality" >:: test_expand_grid_cardinality;
      "experiment_expand_grid_preserves_base_seed"
      >:: test_expand_grid_preserves_base_seed;
      "experiment_best_run_empty" >:: test_best_run_empty;
      "experiment_best_run_picks_max_pnl" >:: test_best_run_picks_max_pnl;
      "experiment_evaluate_policy_hold_only" >:: test_evaluate_policy_hold_only;
    ]
end

module ExtraMetricsTester (M : module type of Metrics) = struct
  let test_metrics_empty_curve _ =
    let m = M.compute ~pnl_curve:[||] ~num_trades:0 ~winning_trades:0 in
    assert_float_eq ~msg:"final_pnl on empty curve" 0.0 m.final_pnl;
    assert_float_eq ~msg:"sharpe on empty curve" 0.0 m.sharpe_ratio;
    assert_float_eq ~msg:"max_drawdown on empty curve" 0.0 m.max_drawdown

  let test_metrics_single_point _ =
    let m =
      M.compute ~pnl_curve:[| 0.0 |] ~num_trades:0 ~winning_trades:0
    in
    assert_float_eq ~msg:"final_pnl single point" 0.0 m.final_pnl;
    assert_float_eq ~msg:"max_drawdown single point" 0.0 m.max_drawdown

  let test_metrics_to_csv_round_trip _ =
    let m =
      M.compute ~pnl_curve:[| 0.0; 5.0; 10.0 |] ~num_trades:2 ~winning_trades:1
    in
    let path = Filename.temp_file "metrics" ".csv" in
    M.to_csv m path;
    let ic = open_in path in
    let header = input_line ic in
    let row = input_line ic in
    close_in ic;
    Sys.remove path;
    assert_bool "header has total_pnl"
      (String.length header > 0 && String.sub header 0 9 = "total_pnl");
    assert_bool "row not empty" (String.length row > 0)

  let test_metrics_win_rate_zero_trades _ =
    let m =
      M.compute ~pnl_curve:[| 0.0; 1.0 |] ~num_trades:0 ~winning_trades:0
    in
    assert_float_eq ~msg:"win_rate=0 when no trades" 0.0 m.win_rate

  let tests =
    [
      "metrics_empty_curve" >:: test_metrics_empty_curve;
      "metrics_single_point" >:: test_metrics_single_point;
      "metrics_to_csv_round_trip" >:: test_metrics_to_csv_round_trip;
      "metrics_win_rate_zero_trades" >:: test_metrics_win_rate_zero_trades;
    ]
end

module ExtraEnvTester
    (E : module type of Environment)
    (C : module type of Config) =
struct
  open E

  let warmup () = max C.long_ma_window C.volatility_window

  let test_max_position_cap _ =
    let env = make_env (warmup () + 60) in
    let _ = reset env in
    for _ = 1 to C.max_position + 5 do
      let _ = step env Buy in
      ()
    done;
    let summary = trade_summary env in
    assert_bool "no closed trades from buys alone"
      (summary.num_closed_trades = 0)

  let test_buy_then_sell_records_trade _ =
    let env = make_env (warmup () + 60) in
    let _ = reset env in
    let _ = step env Buy in
    let _ = step env Sell in
    let summary = trade_summary env in
    assert_equal ~msg:"one closed trade after buy+sell"
      ~printer:string_of_int 1 summary.num_closed_trades

  let test_pnl_matches_sum_of_rewards _ =
    let env = make_env (warmup () + 30) in
    let _ = reset env in
    let total = ref 0.0 in
    for _ = 1 to 10 do
      let r = step env Hold in
      total := !total +. r.reward
    done;
    assert_float_eq ~msg:"pnl equals running sum of rewards" !total (pnl env)

  let test_episode_length_matches_steps _ =
    let env = make_env (warmup () + 25) in
    let _ = reset env in
    let len = episode_length env in
    let count = ref 0 in
    let terminal = ref false in
    while not !terminal do
      let r = step env Hold in
      incr count;
      terminal := r.terminal
    done;
    assert_equal ~msg:"steps until terminal = episode_length"
      ~printer:string_of_int len !count

  let test_reset_restarts_episode _ =
    let env = make_env (warmup () + 40) in
    let _ = reset env in
    let _ = step env Buy in
    let _ = step env Buy in
    let _ = reset env in
    assert_float_eq ~msg:"pnl reset to 0 after second reset" 0.0 (pnl env);
    let s = trade_summary env in
    assert_equal ~msg:"closed trades cleared after reset"
      ~printer:string_of_int 0 s.num_closed_trades

  let tests =
    [
      "extra_env_max_position_cap" >:: test_max_position_cap;
      "extra_env_buy_then_sell_records_trade" >:: test_buy_then_sell_records_trade;
      "extra_env_pnl_matches_sum_of_rewards" >:: test_pnl_matches_sum_of_rewards;
      "extra_env_episode_length_matches_steps"
      >:: test_episode_length_matches_steps;
      "extra_env_reset_restarts_episode" >:: test_reset_restarts_episode;
    ]
end

module ExtraConfigTester (C : module type of Config) = struct
  let test_num_actions_is_three _ =
    assert_equal ~msg:"three actions: sell/hold/buy" ~printer:string_of_int 3
      C.num_actions

  let test_thresholds_strictly_increasing _ =
    let t = C.return_thresholds in
    let ok = ref true in
    for i = 1 to Array.length t - 1 do
      if t.(i) <= t.(i - 1) then ok := false
    done;
    assert_bool "return_thresholds is strictly increasing" !ok

  let test_windows_positive _ =
    assert_bool "short_ma_window > 0" (C.short_ma_window > 0);
    assert_bool "long_ma_window > short_ma_window"
      (C.long_ma_window > C.short_ma_window);
    assert_bool "volatility_window > 0" (C.volatility_window > 0)

  let test_transaction_cost_in_range _ =
    assert_bool "0 <= transaction_cost <= 1"
      (C.transaction_cost >= 0.0 && C.transaction_cost <= 1.0)

  let tests =
    [
      "extra_config_num_actions_is_three" >:: test_num_actions_is_three;
      "extra_config_thresholds_strictly_increasing"
      >:: test_thresholds_strictly_increasing;
      "extra_config_windows_positive" >:: test_windows_positive;
      "extra_config_transaction_cost_in_range"
      >:: test_transaction_cost_in_range;
    ]
end

module MT = MetricsTester (Metrics)
module CT = ConfigTester (Config)
module AT = AgentTester (Agent)
module ET = EnvironmentTester (Environment) (Config)
module ST = SvgPlotTester (Svg_plot)
module TST = TradeStatsTester (Trade_stats)
module BT = BaselinesTester (Baselines)
module PT = PnlPlotTester (Pnl_plot)
module XT = ExperimentTester (Experiment)
module EMT = ExtraMetricsTester (Metrics)
module EET = ExtraEnvTester (Environment) (Config)
module ECT = ExtraConfigTester (Config)

let suite =
  "teamproject3110"
  >::: List.flatten
         [
           MT.tests;
           CT.tests;
           AT.tests;
           ET.tests;
           ST.tests;
           TST.tests;
           BT.tests;
           PT.tests;
           XT.tests;
           EMT.tests;
           EET.tests;
           ECT.tests;
         ]

let () = run_test_tt_main suite
