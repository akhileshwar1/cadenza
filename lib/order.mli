(* lib/order.mli *)

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
  | Partial
  | Unknown
  | Created

  (* Conversion Functions *)

    val string_to_status : string -> status_type

    val status_to_string : status_type -> string

    val generate_order_id: unit -> string

(* Order Entity *)

type t = {
  placed_at : Ptime.t option;
  executed_at : Ptime.t option;
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
  strategy_name : string;
  lot : int;
  filled_quantity : int;
  filled_price : float;
  order_id : string;
  broker_order_id : string;
}

  (* Helper function to convert an Order.t to a Yojson.Safe.t *)
  val json_of_order : t -> Yojson.Safe.t

  val make_order :
  tradingsymbol:string ->
  quantity:int ->
  lots:int ->
  price:float ->
  side:side ->
  strategy_name:string ->
  t list


  val of_yojson : Yojson.Safe.t -> t

  (* val to_yojson : t -> Yojson.Safe.t *)

  val apply_order_update: t -> t -> t
  val ptime_of_string : string -> Ptime.t option
