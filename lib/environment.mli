(** Trading environment: CSV price series-> RL step interface. *)

(** Supported trading actions. *)
type action =
  | Sell
  | Hold
  | Buy

(** Opaque environment; holds the price series, current index, and position. *)
type t

(** Result of one environment transition. *)
type step_result = {
  next_state : int;  (** encoded state after advancing one bar *)
  reward : float;  (** dollar reward produced on the bar *)
  terminal : bool;  (** whether the episode has reached its final bar *)
}

(** Load CSV with columns [Date,Close]. Raise error on incorrect input. *)
val load : string -> t

(** Reset to the first valid step (after warmup for MA/volatility). Returns
    initial state id. *)
val reset : t -> int

(** Apply an action, advance one bar, return next state+reward. *)
val step : t -> action -> step_result

(** Current encoded state id between 0 (inclusive) and [Config.num_states]
    (exclusive). *)
val current_state : t -> int

(** Running PnL in dollars: realized (from closed trades) plus unrealized
    (current position marked to market), net of transaction costs. it starts at
    0; no idea of starting capital. This will be used by Metrics. *)
val pnl : t -> float

(** Closed-trade statistics accumulated since the last [reset]. *)
val trade_summary : t -> Trade_stats.summary

(** Number of usable steps per episode (length minus warmup). *)
val episode_length : t -> int

(** [action_of_int n] decodes an action index into its action. The precondition
    is that [n] is 0, 1, or 2. *)
val action_of_int : int -> action

(** [int_of_action a] is the action index used by the Q-table for [a]. *)
val int_of_action : action -> int
