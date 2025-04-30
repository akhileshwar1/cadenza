(* lib/position.ml *)

type status =
  | Open
  | Closed

type side =
  | Buy
  | Sell

type t = {
  opened_at_epoch : float;
  closed_at_epoch : float;
  symbol : string;
  net_buy_qty : int;
  net_sell_qty: int;
  net_buy_price : float;
  net_sell_price : float;
  current_ask_price: float;
  current_bid_price : float;
  side : side;
  value : float;
  status : status;
}
