(* lib/order.ml *)

(* Basic Order Types *)

type side =
  | Buy
  | Sell

type order_type =
  | Limit
  | Market

type product_type =
  | CNC
  | NRML
  | MIS

type validity_type =
  | DAY
  | IOC

type status_type =
  | Cancelled
  | Pending
  | Rejected
  | Completed
  | Unknown

(* Conversion Functions *)

let string_to_status = function
  | "Cancelled" -> Cancelled
  | "Pending" -> Pending
  | "Rejected" -> Rejected
  | "Completed" -> Completed
  | _ -> Unknown

let status_to_string = function
  | Cancelled -> "Cancelled"
  | Pending -> "Pending"
  | Rejected -> "Rejected"
  | Completed -> "Completed"
  | Unknown -> "Unknown"

(* Order Entity *)

type t = {
  tradingsymbol : string;
  exchange : string;
  quantity : int;
  price : float;
  trigger_price : float;
  side : side;
  order_type : order_type;
  product : product_type;
  validity : validity_type;
  status : status_type option;
}
