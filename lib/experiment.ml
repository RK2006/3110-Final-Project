type hyperparameters = {
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

let describe_hyperparameters h =
  Printf.sprintf
    "alpha=%.4f gamma=%.4f epsilon_start=%.4f epsilon_min=%.4f \
     epsilon_decay=%.4f episodes=%d seed=%d"
    h.alpha h.gamma h.epsilon_start h.epsilon_min h.epsilon_decay h.num_episodes
    h.seed

let evaluation_from_curve env pnl_curve =
  let trade_summary = Environment.trade_summary env in
  let metrics =
    Metrics.compute ~pnl_curve ~num_trades:trade_summary.num_closed_trades
      ~winning_trades:trade_summary.winning_closed_trades
  in
  { pnl_curve; metrics; trade_summary }

let evaluate_policy env policy =
  let ep_len = Environment.episode_length env in
  let pnl_curve = Array.make (ep_len + 1) 0.0 in
  let state = ref (Environment.reset env) in
  let terminal = ref false in
  let step_idx = ref 1 in
  while not !terminal do
    let action = policy !state in
    let result = Environment.step env action in
    pnl_curve.(!step_idx) <- Environment.pnl env;
    incr step_idx;
    state := result.next_state;
    terminal := result.terminal
  done;
  evaluation_from_curve env pnl_curve

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

let train_agent ?progress_every env ~hyperparameters =
  Random.init hyperparameters.seed;
  let agent =
    Agent.create ~num_states:Config.num_states ~num_actions:Config.num_actions
      ~alpha:hyperparameters.alpha ~gamma:hyperparameters.gamma
      ~epsilon:hyperparameters.epsilon_start
  in
  for episode = 1 to hyperparameters.num_episodes do
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
    Agent.decay_epsilon agent ~rate:hyperparameters.epsilon_decay
      ~min_epsilon:hyperparameters.epsilon_min;
    maybe_print_progress ?progress_every episode hyperparameters.num_episodes
      !running_reward (Agent.epsilon agent)
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
    "alpha,gamma,epsilon_start,epsilon_min,epsilon_decay,num_episodes,seed,total_pnl,sharpe_ratio,max_drawdown,win_rate,winning_trades,num_trades,final_pnl\n";
  List.iter
    (fun run ->
      let h = run.hyperparameters in
      let m = run.evaluation.metrics in
      Printf.fprintf oc "%f,%f,%f,%f,%f,%d,%d,%f,%f,%f,%f,%d,%d,%f\n" h.alpha
        h.gamma h.epsilon_start h.epsilon_min h.epsilon_decay h.num_episodes
        h.seed m.total_pnl m.sharpe_ratio m.max_drawdown m.win_rate
        m.winning_trades m.num_trades m.final_pnl)
    runs;
  close_out oc

let print_grid_results runs =
  let ranked = sorted_runs runs in
  Printf.printf
    "\n\
     Grid Search Results\n\
     rank | total_pnl | sharpe | win_rate | alpha | gamma | eps_decay | episodes\n";
  List.iteri
    (fun idx run ->
      let h = run.hyperparameters in
      let m = run.evaluation.metrics in
      Printf.printf
        "%4d | %9.2f | %6.3f | %7.2f%% | %.4f | %.4f | %.4f | %d\n%!" (idx + 1)
        m.total_pnl m.sharpe_ratio (m.win_rate *. 100.0) h.alpha h.gamma
        h.epsilon_decay h.num_episodes)
    ranked
