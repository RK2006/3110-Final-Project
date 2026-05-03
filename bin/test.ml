open Teamproject3110

let rec prompt_file folder prompt =
  Printf.printf "%s%!" prompt;
  let name = String.trim (input_line stdin) in
  let path = folder ^ name in
  if Sys.file_exists path then path
  else begin
    Printf.printf "File '%s' not found. Please try again.\n%!" path;
    prompt_file folder prompt
  end

let run () =
  let data_path =
    prompt_file "data/" "Enter CSV filename (must be in data/): "
  in
  let model_path =
    prompt_file "models/" "Enter model filename (must be in models/): "
  in

  (* ── load ─────────────────────────────────────────────────────────────── *)
  Printf.printf "\nLoading dataset %s...\n%!" data_path;
  let env = Environment.load data_path in
  Printf.printf "Loading model %s...\n%!" model_path;
  let agent = Agent.load model_path in

  Printf.printf "episode_length = %d   num_states = %d\n%!"
    (Environment.episode_length env)
    Config.num_states;

  (* ── greedy evaluation pass ───────────────────────────────────────────── *)
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

  Printf.printf "\n── Q-learning Test Metrics ──\n";
  Metrics.print metrics
