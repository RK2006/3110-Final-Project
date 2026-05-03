open OUnit2
open Teamproject3110

let eps = 1e-6

let assert_float_eq ?(tol = eps) ~msg expected actual =
  assert_bool
    (Printf.sprintf "%s: expected %f, got %f" msg expected actual)
    (Float.abs (expected -. actual) < tol)

(*f unctorized test Modules *)

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
    assert_equal ~msg:"num_trades" 3 m.num_trades

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
    assert_equal ~msg:"num_states matches product" expected C.num_states

  let test_config_no_shorting _ =
    assert_equal ~msg:"position buckets = max_position + 1" (C.max_position + 1)
      C.num_position_buckets

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
    assert_equal ~msg:"greedy picks best action" 2
      (A.act a ~state:0 ~training:false)

  let test_agent_training_epsilon_zero _ =
    let a =
      A.create ~num_states:4 ~num_actions:3 ~alpha:1.0 ~gamma:0.0 ~epsilon:0.0
    in
    A.update a ~state:0 ~action:1 ~reward:2.0 ~next_state:0 ~terminal:true;
    assert_equal ~msg:"epsilon=0 is greedy even in training" 1
      (A.act a ~state:0 ~training:true)

  let test_agent_update_terminal _ =
    let a =
      A.create ~num_states:2 ~num_actions:2 ~alpha:0.5 ~gamma:0.9 ~epsilon:0.0
    in
    A.update a ~state:0 ~action:0 ~reward:10.0 ~next_state:1 ~terminal:true;
    assert_equal ~msg:"updated action becomes greedy" 0
      (A.act a ~state:0 ~training:false)

  let test_agent_update_nonterminal_bootstrap _ =
    let a =
      A.create ~num_states:2 ~num_actions:2 ~alpha:1.0 ~gamma:0.5 ~epsilon:0.0
    in
    A.update a ~state:1 ~action:1 ~reward:4.0 ~next_state:0 ~terminal:true;
    A.update a ~state:0 ~action:0 ~reward:1.0 ~next_state:1 ~terminal:false;
    assert_equal ~msg:"bootstrap update makes action 0 greedy" 0
      (A.act a ~state:0 ~training:false)

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
      (A.act a ~state:1 ~training:false)
      (A.act b ~state:1 ~training:false);
    assert_equal ~msg:"greedy action preserved at state 2"
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
        assert_equal ~msg:"action_of_int . int_of_action = id" a
          (action_of_int (int_of_action a)))
      [ Sell; Hold; Buy ];
    assert_equal ~msg:"Sell = 0" 0 (int_of_action Sell);
    assert_equal ~msg:"Hold = 1" 1 (int_of_action Hold);
    assert_equal ~msg:"Buy = 2" 2 (int_of_action Buy)

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
    assert_equal ~msg:"pnl starts at 0" 0.0 (pnl env);
    assert_equal ~msg:"episode_length = n - 1 - warmup"
      (n - 1 - warmup ())
      (episode_length env);
    assert_equal ~msg:"current_state matches reset state" s (current_state env)

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

module MT = MetricsTester (Metrics)
module CT = ConfigTester (Config)
module AT = AgentTester (Agent)
module ET = EnvironmentTester (Environment) (Config)

let suite =
  "teamproject3110" >::: List.flatten [ MT.tests; CT.tests; AT.tests; ET.tests ]

let () = run_test_tt_main suite
