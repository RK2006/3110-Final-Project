(* State space: 5 * 2 * 5 * 6 = 300. No shorting: position range is
   0..max_position. *)
let num_return_buckets = 5
let num_trend_buckets = 2
let num_volatility_buckets = 5
let max_position = 5
let num_position_buckets = max_position + 1

let num_states =
  num_return_buckets * num_trend_buckets * num_volatility_buckets
  * num_position_buckets

(* Actions: 0 = Sell, 1 = Hold, 2 = Buy *)
let num_actions = 3

(* Feature windows*)
let short_ma_window = 5
let long_ma_window = 20
let volatility_window = 20

(* Bucket thresholds for daily returns(percent change) for splitting into
   buckets *)
let return_thresholds = [| -0.02; -0.005; 0.005; 0.02 |]

(* Volatility buckets are computed relative to the rolling distribution inside
   environment so we have no fixed thresholds here. *)

(*Q-learning hyperparameters *)
let alpha = 0.1
let gamma = 0.95
let epsilon_start = 1.0
let epsilon_min = 0.05
let epsilon_decay = 0.995

(* Training*)
let num_episodes = 1000

(* Trading *)
let transaction_cost = 0.0005 (* fraction of trade value *)
