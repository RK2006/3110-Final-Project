type run_result = {
  pnl_curve : float array;
  num_trades : int;
  winning_trades : int;
}

let run_policy (env : Environment.t) (policy : int -> Environment.action) :
    run_result =
  let _ = Environment.reset env in
  let n = Environment.episode_length env in
  let pnls = ref [] in
  let num_trades = ref 0 in
  let winning_trades = ref 0 in
  let rec loop i =
    if i >= n then ()
    else
      let s = Environment.current_state env in
      let a = policy s in
      let r = Environment.step env a in
      (match a with
      | Environment.Hold -> ()
      | Environment.Buy | Environment.Sell ->
          incr num_trades;
          if r.reward > 0.0 then incr winning_trades);
      pnls := Environment.pnl env :: !pnls;
      if r.terminal then () else loop (i + 1)
  in
  loop 0;
  {
    pnl_curve = Array.of_list (List.rev !pnls);
    num_trades = !num_trades;
    winning_trades = !winning_trades;
  }

let buy_and_hold env =
  let bought = ref 0 in
  run_policy env (fun _ ->
      if !bought >= Config.max_position then Environment.Hold
      else begin
        incr bought;
        Environment.Buy
      end)

let random_policy ~seed env =
  let rng = Random.State.make [| seed |] in
  run_policy env (fun _ ->
      Environment.action_of_int (Random.State.int rng Config.num_actions))

let decode_trend state =
  let np = Config.num_position_buckets in
  let nv = Config.num_volatility_buckets in
  let nt = Config.num_trend_buckets in
  state / (np * nv) mod nt

let sma_crossover env =
  let prev = ref None in
  run_policy env (fun s ->
      let t = decode_trend s in
      let action =
        match !prev with
        | Some 0 when t = 1 -> Environment.Buy
        | Some 1 when t = 0 -> Environment.Sell
        | _ -> Environment.Hold
      in
      prev := Some t;
      action)
