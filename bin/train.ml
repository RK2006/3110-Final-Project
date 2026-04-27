open Teamproject3110

let run () =
  (* ── prompt for dataset ───────────────────────────────────────────────── *)
  let rec prompt_csv () =
    Printf.printf "Enter CSV filename (in data/): %!";
    let name = String.trim (input_line stdin) in
    let path = "data/" ^ name in
    if Sys.file_exists path then path
    else begin
      Printf.printf "File '%s' not found. Please try again.\n%!" path;
      prompt_csv ()
    end
  in
  let data_path = prompt_csv () in

  (* ── environment + agent ──────────────────────────────────────────────── *)
  Printf.printf "Loading %s ...\n%!" data_path;
  let env = Environment.load data_path in
  Printf.printf "episode_length = %d   num_states = %d\n%!"
    (Environment.episode_length env)
    Config.num_states;

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
  let state = ref (Environment.reset env) in
  let terminal = ref false in

  while not !terminal do
    let action_int = Agent.act agent ~state:!state ~training:false in
    let action = Environment.action_of_int action_int in
    let result = Environment.step env action in
    state := result.next_state;
    terminal := result.terminal
  done;

  (* ── metrics ──────────────────────────────────────────────────────────── *)
  let ep_len = Environment.episode_length env in
  let pnl_curve = Array.init (ep_len + 1) (fun _ -> Environment.pnl env) in
  (* rebuild the curve: reset and replay greedy to collect per-step PnL *)
  let state = ref (Environment.reset env) in
  let terminal = ref false in
  let step_idx = ref 1 in
  let num_trades = ref 0 in
  let winning = ref 0 in
  pnl_curve.(0) <- 0.0;

  while not !terminal do
    let action_int = Agent.act agent ~state:!state ~training:false in
    let action = Environment.action_of_int action_int in
    let result = Environment.step env action in
    pnl_curve.(!step_idx) <- Environment.pnl env;
    (* count trades: a Buy or Sell (not Hold) is a trade *)
    if action_int <> Environment.int_of_action Environment.Hold then begin
      incr num_trades;
      let prev = pnl_curve.(!step_idx - 1) in
      if Environment.pnl env > prev then incr winning
    end;
    incr step_idx;
    state := result.next_state;
    terminal := result.terminal
  done;

  let metrics =
    Metrics.compute ~pnl_curve ~num_trades:!num_trades ~winning_trades:!winning
  in

  Printf.printf "\n── Q-learning Evaluation Metrics ──\n";
  Metrics.print metrics;

  (* ── save Q-table and metrics ─────────────────────────────────────────── *)
  let qtable_path = "data/q_table.csv" in
  let metrics_path = "data/metrics.csv" in
  Agent.save agent qtable_path;
  Metrics.to_csv metrics metrics_path;
  Printf.printf "\nQ-table saved to %s\n" qtable_path;
  Printf.printf "Metrics saved to %s\n%!" metrics_path
