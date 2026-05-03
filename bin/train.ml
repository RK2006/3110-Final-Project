open Teamproject3110

let run env =
  (* ── agent ────────────────────────────────────────────────────────────── *)
  let agent =
    Agent.create ~num_states:Config.num_states ~num_actions:Config.num_actions
      ~alpha:Config.alpha ~gamma:Config.gamma ~epsilon:Config.epsilon_start
  in

  (* ── training loop ────────────────────────────────────────────────────── *)
  Printf.printf "\nTraining for %d episodes...\n%!" Config.num_episodes;

  for episode = 1 to Config.num_episodes do
    let state = ref (Environment.reset env) in
    let running_reward = ref 0.0 in
    let terminal = ref false in

    while not !terminal do
      let action_int = Agent.act agent ~state:!state ~training:true in
      let action = Environment.action_of_int action_int in
      let result = Environment.step env action in
      Agent.update agent ~state:!state ~action:action_int ~reward:result.reward
        ~next_state:result.next_state ~terminal:result.terminal;
      running_reward := !running_reward +. result.reward;
      state := result.next_state;
      terminal := result.terminal
    done;

    Agent.decay_epsilon agent ~rate:Config.epsilon_decay
      ~min_epsilon:Config.epsilon_min;

    if episode mod 100 = 0 then
      Printf.printf "Episode %4d/%d  |  reward: %8.4f  |  epsilon: %.4f\n%!"
        episode Config.num_episodes !running_reward (Agent.epsilon agent)
  done;

  (* ── evaluation pass (greedy, no exploration) ─────────────────────────── *)
  Printf.printf "\nRunning evaluation episode (greedy)...\n%!";

  let ep_len = Environment.episode_length env in
  let pnl_curve = Array.make (ep_len + 1) 0.0 in
  let num_trades = ref 0 in
  let winning = ref 0 in
  let state = ref (Environment.reset env) in
  let terminal = ref false in
  let step_idx = ref 1 in
  pnl_curve.(0) <- 0.0;

  while not !terminal do
    let action_int = Agent.act agent ~state:!state ~training:false in
    let action = Environment.action_of_int action_int in
    let result = Environment.step env action in
    pnl_curve.(!step_idx) <- Environment.pnl env;
    if action_int <> 1 (* not Hold *) then begin
      incr num_trades;
      if pnl_curve.(!step_idx) > pnl_curve.(!step_idx - 1) then incr winning
    end;
    incr step_idx;
    state := result.next_state;
    terminal := result.terminal
  done;

  (* ── metrics ──────────────────────────────────────────────────────────── *)
  let metrics =
    Metrics.compute ~pnl_curve ~num_trades:!num_trades ~winning_trades:!winning
  in

  Printf.printf "\n── Q-learning Evaluation Metrics ──\n";
  Metrics.print metrics;

  (* ── save Q-table to models/ ──────────────────────────────────────────── *)
  Printf.printf "\nSave model? (y/n): %!";
  let save_choice = String.trim (input_line stdin) in
  if save_choice = "y" || save_choice = "Y" then begin
    Printf.printf "Enter filename to save model (in models/): %!";
    let model_name = String.trim (input_line stdin) in
    let qtable_path = "models/" ^ model_name in
    Agent.save agent qtable_path;
    Printf.printf "Q-table saved to %s\n%!" qtable_path
  end
  else Printf.printf "Model not saved.\n%!"
