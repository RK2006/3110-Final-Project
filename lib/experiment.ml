type algorithm =
  | Q_learning
  | Sarsa

type hyperparameters = {
  algorithm : algorithm;
  alpha : float;
  gamma : float;
  epsilon_start : float;
  epsilon_min : float;
  epsilon_decay : float;
  num_episodes : int;
  seed : int;
}

type evaluation = {
  pnl_curve : float array;
  metrics : Metrics.t;
  trade_summary : Trade_stats.summary;
  actions : Environment.action array;
}

type trained_run = {
  hyperparameters : hyperparameters;
  agent : Agent.t;
  evaluation : evaluation;
}

type grid = {
  alphas : float list;
  gammas : float list;
  epsilon_decays : float list;
  num_episodes_values : int list;
}

let default_hyperparameters =
  {
    algorithm = Q_learning;
    alpha = Config.alpha;
    gamma = Config.gamma;
    epsilon_start = Config.epsilon_start;
    epsilon_min = Config.epsilon_min;
    epsilon_decay = Config.epsilon_decay;
    num_episodes = Config.num_episodes;
    seed = 42;
  }

let default_grid =
  {
    alphas = [ 0.05; 0.1; 0.2 ];
    gammas = [ 0.9; 0.95; 0.99 ];
    epsilon_decays = [ 0.001; 0.005; 0.01; 0.05 ];
    num_episodes_values = [ 500; 1000; 1500 ];
  }

let string_of_algorithm = function
  | Q_learning -> "q_learning"
  | Sarsa -> "sarsa"

let describe_hyperparameters h =
  Printf.sprintf
    "algorithm=%s alpha=%.4f gamma=%.4f epsilon_start=%.4f epsilon_min=%.4f \
     epsilon_decay=%.4f episodes=%d seed=%d"
    (string_of_algorithm h.algorithm)
    h.alpha h.gamma h.epsilon_start h.epsilon_min h.epsilon_decay h.num_episodes
    h.seed

let evaluation_from_curve env pnl_curve actions =
  let trade_summary = Environment.trade_summary env in
  let metrics =
    Metrics.compute ~pnl_curve ~num_trades:trade_summary.num_closed_trades
      ~winning_trades:trade_summary.winning_closed_trades
  in
  { pnl_curve; metrics; trade_summary; actions }

let evaluate_policy env policy =
  let ep_len = Environment.episode_length env in
  let pnl_curve = Array.make (ep_len + 1) 0.0 in
  let actions = Array.make ep_len Environment.Hold in
  let state = ref (Environment.reset env) in
  let terminal = ref false in
  let step_idx = ref 1 in
  while not !terminal do
    let action = policy !state in
    actions.(!step_idx - 1) <- action;
    let result = Environment.step env action in
    pnl_curve.(!step_idx) <- Environment.pnl env;
    incr step_idx;
    state := result.next_state;
    terminal := result.terminal
  done;
  let actions_taken = Array.sub actions 0 (!step_idx - 1) in
  evaluation_from_curve env pnl_curve actions_taken

let evaluate_agent env agent =
  evaluate_policy env (fun state ->
      let action_int = Agent.act agent ~state ~training:false in
      Environment.action_of_int action_int)

let maybe_print_progress ?progress_every episode total_episodes reward epsilon =
  match progress_every with
  | Some every when every > 0 && episode mod every = 0 ->
      Printf.printf "Episode %4d/%d  |  reward: %8.4f  |  epsilon: %.4f\n%!"
        episode total_episodes reward epsilon
  | _ -> ()

(* One Q-learning episode: pick action, step, update against max_a' Q(s',a'). *)
let run_q_learning_episode env agent =
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
  !running_reward

(* One SARSA episode: sample the next action before updating, so the target
   uses Q(s', a') for the action the on-policy learner will actually take. *)
let run_sarsa_episode env agent =
  let state = ref (Environment.reset env) in
  let action_int = ref (Agent.act agent ~state:!state ~training:true) in
  let running_reward = ref 0.0 in
  let terminal = ref false in
  while not !terminal do
    let action = Environment.action_of_int !action_int in
    let result = Environment.step env action in
    let next_action_int =
      if result.terminal then 0
      else Agent.act agent ~state:result.next_state ~training:true
    in
    Agent.update_sarsa agent ~state:!state ~action:!action_int
      ~reward:result.reward ~next_state:result.next_state
      ~next_action:next_action_int ~terminal:result.terminal;
    running_reward := !running_reward +. result.reward;
    state := result.next_state;
    action_int := next_action_int;
    terminal := result.terminal
  done;
  !running_reward

let train_agent ?progress_every env ~hyperparameters =
  Random.init hyperparameters.seed;
  let agent =
    Agent.create ~num_states:Config.num_states ~num_actions:Config.num_actions
      ~alpha:hyperparameters.alpha ~gamma:hyperparameters.gamma
      ~epsilon:hyperparameters.epsilon_start
  in
  let run_episode =
    match hyperparameters.algorithm with
    | Q_learning -> run_q_learning_episode
    | Sarsa -> run_sarsa_episode
  in
  for episode = 1 to hyperparameters.num_episodes do
    let running_reward = run_episode env agent in
    Agent.decay_epsilon agent ~rate:hyperparameters.epsilon_decay
      ~min_epsilon:hyperparameters.epsilon_min;
    maybe_print_progress ?progress_every episode hyperparameters.num_episodes
      running_reward (Agent.epsilon agent)
  done;
  agent

let train_and_evaluate ?progress_every env ~hyperparameters =
  let agent = train_agent ?progress_every env ~hyperparameters in
  let evaluation = evaluate_agent env agent in
  { hyperparameters; agent; evaluation }

let expand_grid ~base grid =
  List.concat_map
    (fun alpha ->
      List.concat_map
        (fun gamma ->
          List.concat_map
            (fun epsilon_decay ->
              List.map
                (fun num_episodes ->
                  { base with alpha; gamma; epsilon_decay; num_episodes })
                grid.num_episodes_values)
            grid.epsilon_decays)
        grid.gammas)
    grid.alphas

let grid_search env candidates =
  List.map
    (fun hyperparameters -> train_and_evaluate env ~hyperparameters)
    candidates

let run_is_better left right =
  let left_metrics = left.evaluation.metrics in
  let right_metrics = right.evaluation.metrics in
  if left_metrics.total_pnl <> right_metrics.total_pnl then
    left_metrics.total_pnl > right_metrics.total_pnl
  else if left_metrics.sharpe_ratio <> right_metrics.sharpe_ratio then
    left_metrics.sharpe_ratio > right_metrics.sharpe_ratio
  else left_metrics.win_rate > right_metrics.win_rate

let best_run = function
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun best candidate ->
             if run_is_better candidate best then candidate else best)
           first rest)

let sorted_runs runs =
  List.sort
    (fun left right ->
      if run_is_better left right then -1
      else if run_is_better right left then 1
      else 0)
    runs

let grid_results_to_csv runs path =
  let oc = open_out path in
  output_string oc
    "algorithm,alpha,gamma,epsilon_start,epsilon_min,epsilon_decay,num_episodes,seed,total_pnl,sharpe_ratio,max_drawdown,win_rate,winning_trades,num_trades,final_pnl\n";
  List.iter
    (fun run ->
      let h = run.hyperparameters in
      let m = run.evaluation.metrics in
      Printf.fprintf oc "%s,%f,%f,%f,%f,%f,%d,%d,%f,%f,%f,%f,%d,%d,%f\n"
        (string_of_algorithm h.algorithm)
        h.alpha h.gamma h.epsilon_start h.epsilon_min h.epsilon_decay
        h.num_episodes h.seed m.total_pnl m.sharpe_ratio m.max_drawdown
        m.win_rate m.winning_trades m.num_trades m.final_pnl)
    runs;
  close_out oc

let print_grid_results runs =
  let ranked = sorted_runs runs in
  Printf.printf
    "\n\
     Grid Search Results\n\
     rank | algo        | total_pnl | sharpe | win_rate | alpha | gamma | \
     eps_decay | episodes\n";
  List.iteri
    (fun idx run ->
      let h = run.hyperparameters in
      let m = run.evaluation.metrics in
      Printf.printf
        "%4d | %-11s | %9.2f | %6.3f | %7.2f%% | %.4f | %.4f | %.4f | %d\n%!"
        (idx + 1)
        (string_of_algorithm h.algorithm)
        m.total_pnl m.sharpe_ratio (m.win_rate *. 100.0) h.alpha h.gamma
        h.epsilon_decay h.num_episodes)
    ranked
