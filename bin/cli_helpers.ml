open Teamproject3110

let rec prompt_existing_file folder prompt =
  Printf.printf "%s%!" prompt;
  let name = String.trim (input_line stdin) in
  let path = folder ^ name in
  if Sys.file_exists path then path
  else begin
    Printf.printf "File '%s' not found. Please try again.\n%!" path;
    prompt_existing_file folder prompt
  end

let rec prompt_yes_no prompt =
  Printf.printf "%s%!" prompt;
  match String.lowercase_ascii (String.trim (input_line stdin)) with
  | "y" | "yes" -> true
  | "n" | "no" -> false
  | _ ->
      Printf.printf "Please answer y or n.\n%!";
      prompt_yes_no prompt

let rec prompt_string prompt =
  Printf.printf "%s%!" prompt;
  let s = String.trim (input_line stdin) in
  if String.length s = 0 then begin
    Printf.printf "Input cannot be empty. Please try again.\n%!";
    prompt_string prompt
  end
  else s

let rec prompt_algorithm () =
  Printf.printf
    "\n\
     Select algorithm:\n\
    \  [1] Q-learning (off-policy)\n\
    \  [2] SARSA (on-policy)\n\
     > %!";
  match String.trim (input_line stdin) with
  | "1" -> (Experiment.Q_learning, "Q-learning")
  | "2" -> (Experiment.Sarsa, "SARSA")
  | _ ->
      Printf.printf "Invalid choice. Please enter 1 or 2.\n%!";
      prompt_algorithm ()

let maybe_print_pnl_curve pnl_curve =
  if prompt_yes_no "Show ASCII PnL plot? (y/n): " then begin
    print_endline "";
    Pnl_plot.print_ascii pnl_curve
  end

let maybe_save_pnl_curve pnl_curve =
  if prompt_yes_no "Save PnL curve to CSV? (y/n): " then begin
    let path = prompt_string "Enter filename (in outputs/): " in
    let full_path = "outputs/" ^ path in
    try
      Pnl_plot.write_csv pnl_curve full_path;
      Printf.printf "Saved PnL curve to %s\n%!" full_path
    with Sys_error message ->
      Printf.printf "Could not save PnL curve: %s\n%!" message
  end

let maybe_save_svg_plot ?actions ?title pnl_curve =
  if prompt_yes_no "Save SVG chart? (y/n): " then begin
    let path = prompt_string "Enter filename (in outputs/): " in
    let full_path = "outputs/" ^ path in
    let config =
      match title with
      | None -> Svg_plot.default_config
      | Some t -> { Svg_plot.default_config with title = t }
    in
    try
      Svg_plot.write ~config ?actions ~path:full_path pnl_curve;
      Printf.printf "Saved SVG chart to %s\n%!" full_path
    with Sys_error message ->
      Printf.printf "Could not save SVG chart: %s\n%!" message
  end
