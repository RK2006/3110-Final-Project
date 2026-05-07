(** Non-learning policies for comparison against the Q-learning agent. *)

(** Output of a single baseline episode. *)
type run_result = {
  pnl_curve : float array;  (** cumulative PnL, beginning at 0 *)
  num_trades : int;  (** number of closed trades *)
  winning_trades : int;  (** number of profitable closed trades *)
}

(** [buy_and_hold env] buys until the position limit is reached, then holds for
    the rest of the episode. *)
val buy_and_hold : Environment.t -> run_result

(** [random_policy ~seed env] samples each action uniformly at random using
    [seed]. *)
val random_policy : seed:int -> Environment.t -> run_result

(** [sma_crossover env] buys on an upward moving-average crossover and sells on
    a downward crossover. *)
val sma_crossover : Environment.t -> run_result
