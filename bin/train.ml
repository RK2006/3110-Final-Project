open Teamproject3110

let run env =
  let algorithm, algo_label = Cli_helpers.prompt_algorithm () in
  let hyperparameters = { Experiment.default_hyperparameters with algorithm } in
  Printf.printf "\nTraining %s for %d episodes...\n%!" algo_label
    Config.num_episodes;
  let run =
    Experiment.train_and_evaluate env ~hyperparameters ~progress_every:100
  in
  Printf.printf "\nRunning evaluation episode (greedy)...\n%!";
  Printf.printf "\n── %s Evaluation Metrics ──\n" algo_label;
  Metrics.print run.evaluation.metrics;
  Cli_helpers.maybe_print_pnl_curve run.evaluation.pnl_curve;
  Cli_helpers.maybe_save_pnl_curve run.evaluation.pnl_curve;
  Cli_helpers.maybe_save_svg_plot ~actions:run.evaluation.actions
    ~title:(algo_label ^ " Training PnL")
    run.evaluation.pnl_curve;

  if Cli_helpers.prompt_yes_no "\nSave model? (y/n): " then begin
    Printf.printf "Enter filename to save model (in models/): %!";
    let model_name = String.trim (input_line stdin) in
    let qtable_path = "models/" ^ model_name in
    Agent.save run.agent qtable_path;
    Printf.printf "Q-table saved to %s\n%!" qtable_path
  end
  else Printf.printf "Model not saved.\n%!"
