(** Utilities for saving and visualizing cumulative PnL curves. *)

(** [write_csv curve path] writes [curve] to [path] with columns [step,pnl]. *)
val write_csv : float array -> string -> unit

(** [render_ascii ?width ?height curve] is a simple fixed-size ASCII line chart
    for [curve]. *)
val render_ascii : ?width:int -> ?height:int -> float array -> string

(** [print_ascii ?width ?height curve] prints
    [render_ascii ?width ?height curve] to standard output. *)
val print_ascii : ?width:int -> ?height:int -> float array -> unit
