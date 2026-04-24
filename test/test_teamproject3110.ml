open OUnit2
open Teamproject3110

let eps = 1e-6

let assert_float_eq ?(tol = eps) ~msg expected actual =
  assert_bool
    (Printf.sprintf "%s: expected %f, got %f" msg expected actual)
    (Float.abs (expected -. actual) < tol)

(* Metrics *)

(* flat pnl curve -> every metric should be zero. *)
let test_metrics_flat _ =
  let m =
    Metrics.compute ~pnl_curve:[| 0.0; 0.0; 0.0 |] ~num_trades:0
      ~winning_trades:0
  in
  assert_float_eq ~msg:"final_pnl" 0.0 m.final_pnl;
  assert_float_eq ~msg:"max_drawdown" 0.0 m.max_drawdown;
  assert_float_eq ~msg:"sharpe_ratio" 0.0 m.sharpe_ratio;
  assert_float_eq ~msg:"win_rate" 0.0 m.win_rate

(* always-up curve: no drawdown, final = total, all trades count as wins *)
let test_metrics_monotonic_up _ =
  let m =
    Metrics.compute
      ~pnl_curve:[| 0.0; 10.0; 20.0; 30.0 |]
      ~num_trades:3 ~winning_trades:3
  in
  assert_float_eq ~msg:"final_pnl" 30.0 m.final_pnl;
  assert_float_eq ~msg:"total_pnl" 30.0 m.total_pnl;
  assert_float_eq ~msg:"max_drawdown" 0.0 m.max_drawdown;
  assert_float_eq ~msg:"win_rate" 1.0 m.win_rate;
  assert_equal ~msg:"num_trades" 3 m.num_trades

(* drawdown is peak-to-trough (100 ->40 = 60), not end-to-end. *)
let test_metrics_drawdown _ =
  let m =
    Metrics.compute
      ~pnl_curve:[| 0.0; 50.0; 100.0; 40.0; 60.0 |]
      ~num_trades:4 ~winning_trades:2
  in
  assert_float_eq ~msg:"max_drawdown" 60.0 m.max_drawdown;
  assert_float_eq ~msg:"final_pnl" 60.0 m.final_pnl;
  assert_float_eq ~msg:"win_rate" 0.5 m.win_rate

(* pure losses: drawdown is measured off the starting peak of 0 *)
let test_metrics_losses _ =
  let m =
    Metrics.compute ~pnl_curve:[| 0.0; -10.0; -25.0 |] ~num_trades:2
      ~winning_trades:0
  in
  assert_float_eq ~msg:"final_pnl" (-25.0) m.final_pnl;
  assert_float_eq ~msg:"max_drawdown" 25.0 m.max_drawdown;
  assert_float_eq ~msg:"win_rate" 0.0 m.win_rate

(* constant per-step deltas -> std = 0 -> sharpe should fall back to 0 ideally *)
let test_metrics_sharpe_zero_variance _ =
  let m =
    Metrics.compute ~pnl_curve:[| 0.0; 1.0; 2.0; 3.0 |] ~num_trades:3
      ~winning_trades:3
  in
  assert_float_eq ~msg:"sharpe with zero variance" 0.0 m.sharpe_ratio

(* Config *)

(* num_states should just be the product of the four bucket counts. *)
let test_config_state_count _ =
  let expected =
    Config.num_return_buckets * Config.num_trend_buckets
    * Config.num_volatility_buckets * Config.num_position_buckets
  in
  assert_equal ~msg:"num_states matches product" expected Config.num_states

(* no shorting, so positions are 0 to max_position inclusive. *)
let test_config_no_shorting _ =
  assert_equal ~msg:"position buckets = max_position + 1"
    (Config.max_position + 1) Config.num_position_buckets

(* Agent *)

(* create should just stash epsilon as-is. *)
let test_agent_create_epsilon _ =
  let a =
    Agent.create ~num_states:10 ~num_actions:3 ~alpha:0.1 ~gamma:0.9
      ~epsilon:0.3
  in
  assert_float_eq ~msg:"epsilon stored" 0.3 (Agent.epsilon a)

(* act with training=false picks the current argmax. *)
let test_agent_greedy_picks_max _ =
  (* With epsilon=0 and training=false, act is pure greedy. After one update to
     state 0 action 2, action 2 should be best. *)
  let a =
    Agent.create ~num_states:4 ~num_actions:3 ~alpha:1.0 ~gamma:0.0 ~epsilon:0.0
  in
  Agent.update a ~state:0 ~action:2 ~reward:5.0 ~next_state:1 ~terminal:true;
  assert_equal ~msg:"greedy picks best action" 2
    (Agent.act a ~state:0 ~training:false)

(* epsilon=0 should kill exploration even when training=true. *)
let test_agent_training_epsilon_zero _ =
  (* With epsilon=0, training=true should still behave greedily.*)
  let a =
    Agent.create ~num_states:4 ~num_actions:3 ~alpha:1.0 ~gamma:0.0 ~epsilon:0.0
  in
  Agent.update a ~state:0 ~action:1 ~reward:2.0 ~next_state:0 ~terminal:true;
  assert_equal ~msg:"epsilon=0 is greedy even in training" 1
    (Agent.act a ~state:0 ~training:true)

(* a single terminal update should push the taken action to the top. *)
let test_agent_update_terminal _ =
  (* Ttrminal transition: Q(s,a) += alpha * (reward - Q(s,a)). Start from 0 so
     new Q(s,a) = alpha * reward. *)
  let a =
    Agent.create ~num_states:2 ~num_actions:2 ~alpha:0.5 ~gamma:0.9 ~epsilon:0.0
  in
  Agent.update a ~state:0 ~action:0 ~reward:10.0 ~next_state:1 ~terminal:true;
  (* force greediness, action 0 should now be preferred over action 1. *)
  assert_equal ~msg:"updated action becomes greedy" 0
    (Agent.act a ~state:0 ~training:false)

(* non-terminal update actually pulls in gamma * max Q(s'). *)
let test_agent_update_nonterminal_bootstrap _ =
  (* Non-terminal transition with next_state having a known max Q. Q(0,0)=0 +
     alpha* (r+ gamma * max_a Q(1,a)- 0). We seed Q at state 1 via a prior
     terminal update. *)
  let a =
    Agent.create ~num_states:2 ~num_actions:2 ~alpha:1.0 ~gamma:0.5 ~epsilon:0.0
  in
  (*After this: Q(1,1) = 1.0* (4.0 + 0) = 4.0. *)
  Agent.update a ~state:1 ~action:1 ~reward:4.0 ~next_state:0 ~terminal:true;
  (* After this: Q(0,0) = 0 +1.0 * (1.0 + 0.5 * 4.0 - 0) = 3.0. Then Q(0,0)=3.0
     > Q(0,1)=0, so action 0 is greedy. *)
  Agent.update a ~state:0 ~action:0 ~reward:1.0 ~next_state:1 ~terminal:false;
  assert_equal ~msg:"bootstrap update makes action 0 greedy" 0
    (Agent.act a ~state:0 ~training:false)

(* decay shrinks epsilon by the rate and won't go below min_epsilon. *)
let test_agent_decay_epsilon _ =
  let a =
    Agent.create ~num_states:1 ~num_actions:2 ~alpha:0.1 ~gamma:0.9 ~epsilon:1.0
  in
  Agent.decay_epsilon a ~rate:0.1 ~min_epsilon:0.05;
  assert_float_eq ~msg:"decayed once" 0.9 (Agent.epsilon a);
  (*Large rate pushes below min, result should clamp to min. *)
  Agent.decay_epsilon a ~rate:0.99 ~min_epsilon:0.05;
  assert_float_eq ~msg:"clamps to min_epsilon" 0.05 (Agent.epsilon a)

(* save then load should give back an agent with the same greedy policy. *)
let test_agent_save_load_roundtrip _ =
  let a =
    Agent.create ~num_states:3 ~num_actions:2 ~alpha:0.2 ~gamma:0.8 ~epsilon:0.4
  in
  Agent.update a ~state:1 ~action:0 ~reward:7.0 ~next_state:2 ~terminal:true;
  Agent.update a ~state:2 ~action:1 ~reward:(-1.5) ~next_state:0 ~terminal:true;
  let path = Filename.temp_file "qtable" ".csv" in
  Agent.save a path;
  let b = Agent.load path in
  Sys.remove path;
  assert_float_eq ~msg:"epsilon preserved" (Agent.epsilon a) (Agent.epsilon b);
  (* Greedy actions should match after round trip. *)
  assert_equal ~msg:"greedy action preserved at state 1"
    (Agent.act a ~state:1 ~training:false)
    (Agent.act b ~state:1 ~training:false);
  assert_equal ~msg:"greedy action preserved at state 2"
    (Agent.act a ~state:2 ~training:false)
    (Agent.act b ~state:2 ~training:false)

(* Environment *)

(* Build a CSV long enough to clear the warmup (max long_ma_window,
   volatility_window)+ a usable tail, and write it to a temp file*)
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
  (* Deterministic mildly-varying series so volatility > 0. *)
  Array.init n (fun i ->
      100.0 +. float_of_int i +. if i mod 2 = 0 then 0.5 else -0.5)

let warmup () = max Config.long_ma_window Config.volatility_window

(* int_of_action and action_of_int are inverses, with Sell=0, Hold=1, Buy=2. *)
let test_env_action_int_roundtrip _ =
  List.iter
    (fun a ->
      assert_equal ~msg:"action_of_int . int_of_action = id" a
        (Environment.action_of_int (Environment.int_of_action a)))
    [ Environment.Sell; Environment.Hold; Environment.Buy ];
  assert_equal ~msg:"Sell = 0" 0 (Environment.int_of_action Environment.Sell);
  assert_equal ~msg:"Hold = 1" 1 (Environment.int_of_action Environment.Hold);
  assert_equal ~msg:"Buy = 2" 2 (Environment.int_of_action Environment.Buy)

(* out-of-range ints should fail instead of silently picking something. *)
let test_env_action_of_int_invalid _ =
  assert_raises ~msg:"negative index" (Failure "action_of_int: invalid -1")
    (fun () -> Environment.action_of_int (-1));
  assert_raises ~msg:"out-of-range index" (Failure "action_of_int: invalid 3")
    (fun () -> Environment.action_of_int 3)

(* not enough rows to clear the warmup -> load should fail. *)
let test_env_load_too_short _ =
  (* warmup+1 rows is the failure threshold in Environment.load. *)
  let path = write_price_csv (sample_prices (warmup () + 1)) in
  let finally () = Sys.remove path in
  let did_raise =
    try
      let _ = Environment.load path in
      false
    with Failure _ -> true
  in
  finally ();
  assert_bool "load should fail for too-short series" did_raise

(* reset puts us back at zero pnl and episode_length lines up with the
   series. *)
let test_env_load_and_reset _ =
  let n = warmup () + 40 in
  let path = write_price_csv (sample_prices n) in
  let env = Environment.load path in
  Sys.remove path;
  let s = Environment.reset env in
  assert_bool "state id in valid range" (s >= 0 && s < Config.num_states);
  assert_equal ~msg:"pnl starts at 0" 0.0 (Environment.pnl env);
  assert_equal ~msg:"episode_length = n - 1 - warmup"
    (n - 1 - warmup ())
    (Environment.episode_length env);
  assert_equal ~msg:"current_state matches reset state" s
    (Environment.current_state env)

(* holding with no position shouldn't move pnl or charge a cost. *)
let test_env_hold_zero_position_zero_reward _ =
  (* With position 0 and a Hold action, reward = 0 and no cost. *)
  let path = write_price_csv (sample_prices (warmup () + 40)) in
  let env = Environment.load path in
  Sys.remove path;
  let _ = Environment.reset env in
  let r = Environment.step env Environment.Hold in
  assert_float_eq ~msg:"hold at pos 0 yields zero reward" 0.0 r.reward;
  assert_float_eq ~msg:"pnl unchanged" 0.0 (Environment.pnl env)

(* selling from flat is no position change, no cost. *)
let test_env_sell_at_zero_is_noop _ =
  (* Selling with position 0 should not change position and not casue cost. *)
  let path = write_price_csv (sample_prices (warmup () + 40)) in
  let env = Environment.load path in
  Sys.remove path;
  let _ = Environment.reset env in
  let r = Environment.step env Environment.Sell in
  assert_float_eq ~msg:"sell at pos 0 is costless" 0.0 r.reward

(* pnl should move by exactly the step reward after a Buy. *)
let test_env_buy_incurs_cost _ =
  (* A Buy from pos 0 moves to pos 1 and pays transaction_cost * price. Reward =
     position_after * price_delta - cost = 1 * delta - cost. *)
  let path = write_price_csv (sample_prices (warmup () + 40)) in
  let env = Environment.load path in
  Sys.remove path;
  let _ = Environment.reset env in
  let before = Environment.pnl env in
  let r = Environment.step env Environment.Buy in
  (* PnL should have moved by exactly the reward this step. *)
  assert_float_eq ~msg:"pnl delta equals step reward" r.reward
    (Environment.pnl env -. before)

let test_env_terminal_at_end _ =
  (* After going thru episode_length steps we should land on terminal. *)
  let path = write_price_csv (sample_prices (warmup () + 10)) in
  let env = Environment.load path in
  Sys.remove path;
  let _ = Environment.reset env in
  let len = Environment.episode_length env in
  let last_terminal = ref false in
  for _ = 1 to len do
    let r = Environment.step env Environment.Hold in
    last_terminal := r.terminal
  done;
  assert_bool "final step is terminal" !last_terminal

let test_env_state_in_range_after_steps _ =
  let path = write_price_csv (sample_prices (warmup () + 30)) in
  let env = Environment.load path in
  Sys.remove path;
  let _ = Environment.reset env in
  for _ = 1 to 10 do
    let r = Environment.step env Environment.Buy in
    assert_bool "next_state in range"
      (r.next_state >= 0 && r.next_state < Config.num_states)
  done

let suite =
  "teamproject3110"
  >::: [
         "metrics_flat" >:: test_metrics_flat;
         "metrics_monotonic_up" >:: test_metrics_monotonic_up;
         "metrics_drawdown" >:: test_metrics_drawdown;
         "metrics_losses" >:: test_metrics_losses;
         "metrics_sharpe_zero_variance" >:: test_metrics_sharpe_zero_variance;
         "config_state_count" >:: test_config_state_count;
         "config_no_shorting" >:: test_config_no_shorting;
         "agent_create_epsilon" >:: test_agent_create_epsilon;
         "agent_greedy_picks_max" >:: test_agent_greedy_picks_max;
         "agent_training_epsilon_zero" >:: test_agent_training_epsilon_zero;
         "agent_update_terminal" >:: test_agent_update_terminal;
         "agent_update_nonterminal_bootstrap"
         >:: test_agent_update_nonterminal_bootstrap;
         "agent_decay_epsilon" >:: test_agent_decay_epsilon;
         "agent_save_load_roundtrip" >:: test_agent_save_load_roundtrip;
         "env_action_int_roundtrip" >:: test_env_action_int_roundtrip;
         "env_action_of_int_invalid" >:: test_env_action_of_int_invalid;
         "env_load_too_short" >:: test_env_load_too_short;
         "env_load_and_reset" >:: test_env_load_and_reset;
         "env_hold_zero_position_zero_reward"
         >:: test_env_hold_zero_position_zero_reward;
         "env_sell_at_zero_is_noop" >:: test_env_sell_at_zero_is_noop;
         "env_buy_incurs_cost" >:: test_env_buy_incurs_cost;
         "env_terminal_at_end" >:: test_env_terminal_at_end;
         "env_state_in_range_after_steps"
         >:: test_env_state_in_range_after_steps;
       ]

let () = run_test_tt_main suite
