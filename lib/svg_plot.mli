(** Self-contained SVG plotter for PnL curves with optional trade markers. *)

(** Layout configuration for a rendered SVG. *)
type config = {
  width : int;  (** total SVG width in pixels *)
  height : int;  (** total SVG height in pixels *)
  margin_left : int;  (** left margin reserved for the y-axis labels *)
  margin_right : int;  (** right margin *)
  margin_top : int;  (** top margin reserved for the title *)
  margin_bottom : int;  (** bottom margin reserved for the x-axis labels *)
  title : string;  (** plot title displayed at the top *)
}

(** A reasonable default 900x500 layout titled "PnL Curve". *)
val default_config : config

(** [render ?config ?actions curve] returns a self-contained SVG document
    showing [curve] with axes, gridlines, and a zero line. If [actions] is
    provided it should have length [Array.length curve - 1]; element [i] is
    the action that produced [curve.(i + 1)] and is rendered as a buy / sell
    marker. *)
val render :
  ?config:config -> ?actions:Environment.action array -> float array -> string

(** [write ?config ?actions ~path curve] writes the rendered SVG to [path].
    Raises [Sys_error] if the file cannot be opened. *)
val write :
  ?config:config ->
  ?actions:Environment.action array ->
  path:string ->
  float array ->
  unit
