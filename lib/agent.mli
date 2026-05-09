(** Tabular Q-learning / SARSA agent.

    The same Q-table backs both update rules; the choice of algorithm is
    determined by which update function the training loop calls. *)

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

(** One Q-learning (off-policy) update: Q(s,a) <- Q(s,a) + alpha * (r + gamma *
    max_a' Q(s',a') * (1 - terminal)
    - Q(s,a)) *)
val update :
  t ->
  state:int ->
  action:int ->
  reward:float ->
  next_state:int ->
  terminal:bool ->
  unit

(** One SARSA (on-policy) update: Q(s,a) <- Q(s,a) + alpha * (r + gamma * Q(s',
    a') * (1 - terminal)
    - Q(s,a)) where [next_action] is the action actually selected by the agent's
      current epsilon-greedy policy in [next_state]. *)
val update_sarsa :
  t ->
  state:int ->
  action:int ->
  reward:float ->
  next_state:int ->
  next_action:int ->
  terminal:bool ->
  unit

val decay_epsilon : t -> rate:float -> min_epsilon:float -> unit
val epsilon : t -> float

(** Serialize/deserialize the Q-table to disk; we will use CSV for
    inspectability*)
val save : t -> string -> unit

val load : string -> t
