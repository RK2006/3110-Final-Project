(** Trading environment: CSV price series-> RL step interface. *)

type action = Sell | Hold | Buy

(** Opaque environment; holds the price series, current index, and position. *)
type t

type step_result = {
  next_state : int;
  reward : float;
  terminal : bool;
}

(** Load CSV with columns [Date,Close]. Raise error on incorrect input. *)
val load : string -> t

(** Reset to the first valid step (after warmup for MA/volatility).
    Returns initial state id. *)
val reset : t -> int

(** Apply an action, advance one bar, return next state+reward. *)
val step : t -> action -> step_result

(** Current encoded state id in [0, Config.num_states). *)
val current_state : t -> int

(** Running PnL in dollars: realized (from closed trades) plus unrealized
    (current position marked to market), net of transaction costs. it starts
    at 0; no idea of starting capital. This will be used by Metrics. *)
val pnl : t -> float

(** Number of usable steps per episode (length minus warmup). *)
val episode_length : t -> int

val action_of_int : int -> action
val int_of_action : action -> int
