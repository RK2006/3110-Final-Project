type summary = {
  num_closed_trades : int;
  winning_closed_trades : int;
  realized_pnl : float;
}

type t = {
  mutable open_lots : float list;
  mutable num_closed_trades : int;
  mutable winning_closed_trades : int;
  mutable realized_pnl : float;
}

let create () =
  {
    open_lots = [];
    num_closed_trades = 0;
    winning_closed_trades = 0;
    realized_pnl = 0.0;
  }

let reset t =
  t.open_lots <- [];
  t.num_closed_trades <- 0;
  t.winning_closed_trades <- 0;
  t.realized_pnl <- 0.0

let validate_inputs ~delta_position ~price ~transaction_cost =
  if price <= 0.0 then invalid_arg "record_fill: price must be positive";
  if transaction_cost < 0.0 || transaction_cost > 1.0 then
    invalid_arg "record_fill: transaction_cost must be in [0, 1]";
  if delta_position <> -1 && delta_position <> 0 && delta_position <> 1 then
    invalid_arg "record_fill: delta_position must be -1, 0, or 1"

let entry_cost ~price ~transaction_cost = price *. (1.0 +. transaction_cost)
let exit_value ~price ~transaction_cost = price *. (1.0 -. transaction_cost)

let record_fill t ~delta_position ~price ~transaction_cost =
  validate_inputs ~delta_position ~price ~transaction_cost;
  match delta_position with
  | 0 -> ()
  | 1 ->
      let lot = entry_cost ~price ~transaction_cost in
      t.open_lots <- t.open_lots @ [ lot ]
  | -1 -> (
      match t.open_lots with
      | [] -> invalid_arg "record_fill: sell exceeds open lots"
      | entry :: rest ->
          let profit = exit_value ~price ~transaction_cost -. entry in
          t.open_lots <- rest;
          t.num_closed_trades <- t.num_closed_trades + 1;
          if profit > 0.0 then
            t.winning_closed_trades <- t.winning_closed_trades + 1;
          t.realized_pnl <- t.realized_pnl +. profit)
  | _ -> invalid_arg "record_fill: delta_position must be -1, 0, or 1"

let summary t =
  {
    num_closed_trades = t.num_closed_trades;
    winning_closed_trades = t.winning_closed_trades;
    realized_pnl = t.realized_pnl;
  }
