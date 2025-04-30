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
  qty : int;
  price : float;
  side : side;
  value : float;
  status : status;
}
