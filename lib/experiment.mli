(** Shared training, evaluation, and grid-search helpers for experiments. *)

(** Q-learning settings for one training run. *)
type hyperparameters = {
  alpha : float;
  gamma : float;
  epsilon_start : float;
  epsilon_min : float;
  epsilon_decay : float;
  num_episodes : int;
  seed : int;
}

(** Result of evaluating a policy or trained agent. *)
type evaluation = {
  pnl_curve : float array;  (** cumulative PnL, beginning at 0 *)
  metrics : Metrics.t;  (** aggregate metrics derived from [pnl_curve] *)
  trade_summary : Trade_stats.summary;  (** closed-trade accounting *)
  actions : Environment.action array;
      (** action taken at each step; element [i] produced [pnl_curve.(i + 1)] *)
}

(** Complete output of one train-then-evaluate cycle. *)
type trained_run = {
  hyperparameters : hyperparameters;
  agent : Agent.t;
  evaluation : evaluation;
}

(** Cartesian search space for a simple hyperparameter sweep. *)
type grid = {
  alphas : float list;
  gammas : float list;
  epsilon_decays : float list;
  num_episodes_values : int list;
}

(** [default_hyperparameters] mirrors the current single-run training config. *)
val default_hyperparameters : hyperparameters

(** [default_grid] is a small built-in search space for alpha, gamma,
    epsilon-decay, and episode count. *)
val default_grid : grid

(** [describe_hyperparameters h] is a short human-readable summary of [h]. *)
val describe_hyperparameters : hyperparameters -> string

(** [evaluate_policy env policy] runs one episode in [env] using [policy] and
    returns its PnL curve and metrics. The policy consumes encoded states. *)
val evaluate_policy : Environment.t -> (int -> Environment.action) -> evaluation

(** [evaluate_agent env agent] greedily evaluates [agent] in [env]. *)
val evaluate_agent : Environment.t -> Agent.t -> evaluation

(** [train_agent ?progress_every env ~hyperparameters] trains a fresh agent in
    [env] using [hyperparameters]. If [progress_every] is provided, progress is
    printed every that many episodes. *)
val train_agent :
  ?progress_every:int ->
  Environment.t ->
  hyperparameters:hyperparameters ->
  Agent.t

(** [train_and_evaluate ?progress_every env ~hyperparameters] trains a fresh
    agent and then evaluates it greedily in [env]. *)
val train_and_evaluate :
  ?progress_every:int ->
  Environment.t ->
  hyperparameters:hyperparameters ->
  trained_run

(** [expand_grid ~base grid] produces the cartesian product of [grid], filling
    in non-varied fields from [base]. *)
val expand_grid : base:hyperparameters -> grid -> hyperparameters list

(** [grid_search env candidates] trains and evaluates one fresh agent per
    candidate configuration. *)
val grid_search : Environment.t -> hyperparameters list -> trained_run list

(** [best_run runs] is the run with the highest total PnL, breaking ties by
    Sharpe ratio then win rate. *)
val best_run : trained_run list -> trained_run option

(** [grid_results_to_csv runs path] writes a CSV summary of [runs] to [path]. *)
val grid_results_to_csv : trained_run list -> string -> unit

(** [print_grid_results runs] prints [runs] sorted from best to worst. *)
val print_grid_results : trained_run list -> unit
