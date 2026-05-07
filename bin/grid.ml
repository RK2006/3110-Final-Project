open Teamproject3110

let run () =
  let data_path =
    Cli_helpers.prompt_existing_file "data/"
      "Enter CSV filename (must be in data/): "
  in
  Printf.printf "\nLoading %s ...\n%!" data_path;
  let env = Environment.load data_path in
  Printf.printf "episode_length = %d   num_states = %d\n%!"
    (Environment.episode_length env)
    Config.num_states;

  let candidates =
    Experiment.expand_grid ~base:Experiment.default_hyperparameters
      Experiment.default_grid
  in
  let total = List.length candidates in
  Printf.printf "\nRunning grid search over %d configurations...\n%!" total;

  let runs =
    List.mapi
      (fun idx hyperparameters ->
        Printf.printf "[%d/%d] %s\n%!" (idx + 1) total
          (Experiment.describe_hyperparameters hyperparameters);
        Experiment.train_and_evaluate env ~hyperparameters)
      candidates
  in

  Experiment.print_grid_results runs;

  if Cli_helpers.prompt_yes_no "\nSave full grid results to CSV? (y/n): " then begin
    Printf.printf "Enter output path (for example outputs/grid.csv): %!";
    let path = String.trim (input_line stdin) in
    try
      Experiment.grid_results_to_csv runs path;
      Printf.printf "Saved grid results to %s\n%!" path
    with Sys_error message ->
      Printf.printf "Could not save grid results: %s\n%!" message
  end;

  match Experiment.best_run runs with
  | None -> Printf.printf "No runs to report.\n%!"
  | Some best ->
      Printf.printf "\nBest config: %s\n%!"
        (Experiment.describe_hyperparameters best.hyperparameters);
      Printf.printf "\n── Best run metrics ──\n";
      Metrics.print best.evaluation.metrics;
      Cli_helpers.maybe_print_pnl_curve best.evaluation.pnl_curve;
      Cli_helpers.maybe_save_pnl_curve best.evaluation.pnl_curve;
      if Cli_helpers.prompt_yes_no "\nSave best model? (y/n): " then begin
        Printf.printf "Enter filename to save model (in models/): %!";
        let model_name = String.trim (input_line stdin) in
        let qtable_path = "models/" ^ model_name in
        Agent.save best.agent qtable_path;
        Printf.printf "Q-table saved to %s\n%!" qtable_path
      end
      else Printf.printf "Best model not saved.\n%!"
