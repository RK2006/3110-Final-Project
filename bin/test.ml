open Teamproject3110

let run () =
  let data_path =
    Cli_helpers.prompt_existing_file "data/"
      "Enter CSV filename (must be in data/): "
  in
  let model_path =
    Cli_helpers.prompt_existing_file "models/"
      "Enter model filename (must be in models/): "
  in

  (* ── load ─────────────────────────────────────────────────────────────── *)
  Printf.printf "\nLoading dataset %s...\n%!" data_path;
  let env = Environment.load data_path in
  Printf.printf "Loading model %s...\n%!" model_path;
  let agent = Agent.load model_path in

  Printf.printf "episode_length = %d   num_states = %d\n%!"
    (Environment.episode_length env)
    Config.num_states;

  Printf.printf "\nRunning evaluation episode (greedy)...\n%!";
  let evaluation = Experiment.evaluate_agent env agent in

  Printf.printf "\n── Q-learning Test Metrics ──\n";
  Metrics.print evaluation.metrics;
  Cli_helpers.maybe_print_pnl_curve evaluation.pnl_curve;
  Cli_helpers.maybe_save_pnl_curve evaluation.pnl_curve
