(** Non-learning policies for comparison against the Q-learning agent
    Each runs a full pass over an Environment and returns the PnL curve
    plus trade counts, matching whatever Metrics.compute expect *)

type run_result = {
  pnl_curve : float array;
  num_trades : int;
  winning_trades : int;
}

val buy_and_hold : Environment.t -> run_result
val random_policy : seed:int -> Environment.t -> run_result
val sma_crossover : Environment.t -> run_result
