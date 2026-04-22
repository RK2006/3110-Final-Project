(** Tabular Q-learning agent.*)

type t

val create :
  num_states:int ->
  num_actions:int ->
  alpha:float ->
  gamma:float ->
  epsilon:float ->
  t

(** Pick an action. [training=true] uses epsilon-greedy, and [false] is greedy.
*)
val act : t -> state:int -> training:bool -> int

(** One Q-learning update: Q(s,a) < Q(s,a) +alpha *(r + gamma * max_a' Q(s',a')*
    (1 - terminal)-Q(s,a)) *)
val update :
  t ->
  state:int ->
  action:int ->
  reward:float ->
  next_state:int ->
  terminal:bool ->
  unit

val decay_epsilon : t -> rate:float -> min_epsilon:float -> unit
val epsilon : t -> float

(** Serialize/deserialize the Q-table to disk; we will use CSV for
    inspectability*)
val save : t -> string -> unit

val load : string -> t
