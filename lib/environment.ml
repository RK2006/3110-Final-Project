type action =
  | Sell
  | Hold
  | Buy

let int_of_action = function
  | Sell -> 0
  | Hold -> 1
  | Buy -> 2

let action_of_int = function
  | 0 -> Sell
  | 1 -> Hold
  | 2 -> Buy
  | n -> failwith (Printf.sprintf "action_of_int: invalid %d" n)

type step_result = {
  next_state : int;
  reward : float;
  terminal : bool;
}

type t = {
  prices : float array;
  volatilities : float array;
  vol_thresholds : float array;
  warmup : int;
  mutable idx : int;
  mutable position : int;
  mutable pnl : float;
  trade_stats : Trade_stats.t;
}

let load_prices path =
  let ic = open_in path in
  let _header = input_line ic in
  let acc = ref [] in
  (try
     while true do
       let line = String.trim (input_line ic) in
       if String.length line > 0 then
         match String.split_on_char ',' line with
         | [ _date; close ] ->
             acc := float_of_string (String.trim close) :: !acc
         | _ -> failwith ("Bad csv line: " ^ line)
     done
   with End_of_file -> ());
  close_in ic;
  Array.of_list (List.rev !acc)

let sma prices ~window i =
  let s = ref 0.0 in
  for k = i - window + 1 to i do
    s := !s +. prices.(k)
  done;
  !s /. float_of_int window

(*std of daily simple returns of window ending at i*)
let rolling_vol prices ~window i =
  let rets =
    Array.init window (fun k ->
        let j = i - window + 1 + k in
        (prices.(j) -. prices.(j - 1)) /. prices.(j - 1))
  in
  let m = Array.fold_left ( +. ) 0.0 rets /. float_of_int window in
  let s = Array.fold_left (fun acc r -> acc +. ((r -. m) ** 2.0)) 0.0 rets in
  sqrt (s /. float_of_int window)

(*Sorts xs, returns 4 cut points that split it into 5 quintiles*)
let quintile_thresholds xs =
  let a = Array.copy xs in
  Array.sort compare a;
  let n = Array.length a in
  Array.init 4 (fun k ->
      let i = min (n - 1) ((k + 1) * n / 5) in
      a.(i))

(*returns bucket index for value v given the sorted thresholds*)
let bucket_of thresholds v =
  let b = ref 0 in
  for k = 0 to Array.length thresholds - 1 do
    if v >= thresholds.(k) then b := k + 1
  done;
  !b

let return_bucket r = bucket_of Config.return_thresholds r

(*returns 1 if short MA is abvoe long MA at i, otherwise 0.*)
let trend_bucket prices i =
  let s = sma prices ~window:Config.short_ma_window i in
  let l = sma prices ~window:Config.long_ma_window i in
  if s > l then 1 else 0

(* puts the four features (returns, trends, vol, position) into a single integer
   using mixed-radix math*)
let encode_state ~returns_b ~trend_b ~vol_b ~pos_b =
  let np = Config.num_position_buckets in
  let nv = Config.num_volatility_buckets in
  let nt = Config.num_trend_buckets in
  (((((returns_b * nt) + trend_b) * nv) + vol_b) * np) + pos_b

(*computes current day's return, trend, vol bucket, position and calls
  encode_state to get state id for index t.idx*)
let state_at t =
  let i = t.idx in
  let r = (t.prices.(i) -. t.prices.(i - 1)) /. t.prices.(i - 1) in
  encode_state ~returns_b:(return_bucket r) ~trend_b:(trend_bucket t.prices i)
    ~vol_b:(bucket_of t.vol_thresholds t.volatilities.(i))
    ~pos_b:t.position

let load path =
  let prices = load_prices path in
  let n = Array.length prices in
  let warmup = max Config.long_ma_window Config.volatility_window in
  if n <= warmup + 1 then
    failwith (Printf.sprintf "Not enough price rows: %d" n);
  let volatilities = Array.make n 0.0 in
  for i = warmup to n - 1 do
    volatilities.(i) <- rolling_vol prices ~window:Config.volatility_window i
  done;
  let sample = Array.sub volatilities warmup (n - warmup) in
  let vol_thresholds = quintile_thresholds sample in
  {
    prices;
    volatilities;
    vol_thresholds;
    warmup;
    idx = warmup;
    position = 0;
    pnl = 0.0;
    trade_stats = Trade_stats.create ();
  }

let reset t =
  t.idx <- t.warmup;
  t.position <- 0;
  t.pnl <- 0.0;
  Trade_stats.reset t.trade_stats;
  state_at t

(*core RL loop that gets called every bar. executes trade, computes reward,
  advanced time, updates pnl To computee reward, reward = position * (next_price
  - current_price). for example if you hold 3 shares and price goes up $1,
  reward is $3.*)
let step t action =
  let price = t.prices.(t.idx) in
  let cost = ref 0.0 in
  (match action with
  | Buy when t.position < Config.max_position ->
      t.position <- t.position + 1;
      cost := Config.transaction_cost *. price;
      Trade_stats.record_fill t.trade_stats ~delta_position:1 ~price
        ~transaction_cost:Config.transaction_cost
  | Sell when t.position > 0 ->
      t.position <- t.position - 1;
      cost := Config.transaction_cost *. price;
      Trade_stats.record_fill t.trade_stats ~delta_position:(-1) ~price
        ~transaction_cost:Config.transaction_cost
  | _ -> ());
  let n = Array.length t.prices in
  let next_idx = t.idx + 1 in
  let reward =
    if next_idx >= n then -. !cost
    else (float_of_int t.position *. (t.prices.(next_idx) -. price)) -. !cost
  in
  t.pnl <- t.pnl +. reward;
  t.idx <- min next_idx (n - 1);
  let terminal = t.idx >= n - 1 in
  { next_state = state_at t; reward; terminal }

let current_state t = state_at t
let pnl t = t.pnl
let trade_summary t = Trade_stats.summary t.trade_stats
let episode_length t = Array.length t.prices - 1 - t.warmup
