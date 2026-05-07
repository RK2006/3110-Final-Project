(** Evaluation metrics computed from a PnL curve. starts at 0 and can go
    negative. *)

(** Summary of one full evaluation episode. *)
type t = {
  total_pnl : float;
  sharpe_ratio : float;  (** from per-step PnL deltas *)
  max_drawdown : float;  (** largest peak-to-trough drop in PnL, in dollars *)
  win_rate : float;
      (** fraction of closed buy->sell trades that were profitable *)
  winning_trades : int;  (** number of profitable closed trades *)
  num_trades : int;  (** number of closed trades *)
  final_pnl : float;  (** last point on the cumulative PnL curve *)
}

(** [compute ~pnl_curve ~num_trades ~winning_trades] summarizes an evaluation
    run. The precondition is that [pnl_curve.(0) = 0] whenever [pnl_curve] is
    non-empty. *)
val compute : pnl_curve:float array -> num_trades:int -> winning_trades:int -> t

(** [to_csv m path] writes [m] to [path] as a one-row CSV file. *)
val to_csv : t -> string -> unit

(** [print m] prints a human-readable report for [m]. *)
val print : t -> unit
