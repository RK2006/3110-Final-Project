(** Closed-trade accounting for the long-only trading environment. *)

(** Summary of closed trades recorded so far. *)
type summary = {
  num_closed_trades : int;  (** number of completed buy->sell round trips *)
  winning_closed_trades : int;  (** number of profitable completed trades *)
  realized_pnl : float;  (** realized profit from all completed trades *)
}

(** Opaque accumulator for trade lots and closed-trade outcomes. *)
type t

(** [create ()] is a fresh accumulator with no open lots and zero realized
    profit. *)
val create : unit -> t

(** [reset t] clears all open lots and closed-trade statistics from [t]. *)
val reset : t -> unit

(** [record_fill t ~delta_position ~price ~transaction_cost] records one
    executed share at [price]. A buy is represented by [delta_position = 1], a
    sell by [delta_position = -1], and [0] is a no-op.

    The precondition is that [price > 0], [0 <= transaction_cost <= 1], and a
    sell never exceeds the number of open lots being tracked. *)
val record_fill :
  t -> delta_position:int -> price:float -> transaction_cost:float -> unit

(** [summary t] is the current realized-trade summary of [t]. *)
val summary : t -> summary
