open Teamproject3110

let rec prompt_csv () =
  Printf.printf "Enter CSV filename (in data/): %!";
  let name = String.trim (input_line stdin) in
  let path = "data/" ^ name in
  if Sys.file_exists path then path
  else begin
    Printf.printf "File '%s' not found. Please try again.\n%!" path;
    prompt_csv ()
  end

let run_baseline name result =
  let m =
    Metrics.compute ~pnl_curve:result.Baselines.pnl_curve
      ~num_trades:result.Baselines.num_trades
      ~winning_trades:result.Baselines.winning_trades
  in
  Printf.printf "\n %s \n" name;
  Metrics.print m;
  m

let rec prompt_mode () =
  Printf.printf
    "\n\
     Select mode:\n\
    \  [1] Train a new model\n\
    \  [2] Test an existing model\n\
     > %!";
  match String.trim (input_line stdin) with
  | "1" -> `Train
  | "2" -> `Test
  | _ ->
      Printf.printf "Invalid choice. Please enter 1 or 2.\n%!";
      prompt_mode ()

let () =
  match prompt_mode () with
  | `Train ->
      let data_path = prompt_csv () in
      Printf.printf "Loading %s ...\n%!" data_path;
      let env = Environment.load data_path in
      Printf.printf "episode_length = %d   num_states = %d\n%!"
        (Environment.episode_length env)
        Config.num_states;

      let _ = Environment.reset env in
      let bh = Baselines.buy_and_hold env in
      let _ = run_baseline "Buy and Hold" bh in

      let _ = Environment.reset env in
      let rp = Baselines.random_policy ~seed:42 env in
      let _ = run_baseline "Random (seed=42)" rp in

      let _ = Environment.reset env in
      let sma = Baselines.sma_crossover env in
      let _ = run_baseline "SMA Crossover (5 vs 20)" sma in

      Train.run env
  | `Test -> Test.run ()
