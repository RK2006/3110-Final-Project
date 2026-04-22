(** Evaluation metrics computed from a PnL curve. starts at 0 and can go
    negative. *)

type t = {
  total_pnl : float;
  sharpe_ratio : float;  (** from per-step PnL deltas *)
  max_drawdown : float;  (** largest peak-to-trough drop in PnL, in dollars *)
  win_rate : float;
      (** fraction of closed buy->sell trades that were profitable *)
  num_trades : int;
  final_pnl : float;
}

(** pnl_curve[0] should be 0. *)
val compute : pnl_curve:float array -> num_trades:int -> winning_trades:int -> t

val to_csv : t -> string -> unit
val print : t -> unit
