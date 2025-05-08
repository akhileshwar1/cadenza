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
  strategy_name : string;
  lot : int;
}

(* Helper function to convert an Order.t to a Yojson.Safe.t *)
let json_of_order (order : t) : Yojson.Safe.t =
  `Assoc [
    ("tradingsymbol", `String order.tradingsymbol);
    ("side", `String (match order.side with | Buy -> "Buy" | Sell -> "Sell"));
    ("validity", `String (match order.validity with | DAY -> "DAY" | IOC -> "IOC"));
    ("product", `String (match order.product with | MIS -> "MIS" | CNC -> "CNC" | NRML -> "NRML"));
    ("quantity", `Int order.quantity);
    ("lot", `Int order.lot);
    ("price", `Float order.price);
    ("trigger_price", `Float 0.1);
    ("order_type", `String "Limit");
    ("exchange", `String order.exchange);
    ("strategy_name", `String order.strategy_name)
  ]

let make_order
  ~(tradingsymbol : string)
  ~(quantity : int)
  ~(lots : int)
  ~(price : float)
  ~(side : side)
  ~(strategy_name : string)
  : t =
  {
    tradingsymbol;
    exchange = "NSE";
    quantity;
    price;
    trigger_price = 0.0;
    side;
    order_type = Market;
    product = CNC;
    validity = DAY;
    status = Some Pending;
    lot = lots;
    strategy_name;
  }

let of_yojson (json : Yojson.Safe.t) : t =
  Printf.printf "in yojson\n%!";

  let open Yojson.Safe.Util in
  let safe f key =
    try f (json |> member key)
    with e ->
      Printf.printf "Error extracting key '%s': %s\n%!" key (Printexc.to_string e);
      raise e
  in

  let safe_match key f = 
    try match f (json |> member key) with
      | "Buy" -> Buy
      | "Sell" -> Sell
      | other -> Printf.printf "Unknown side value '%s', defaulting to Buy\n%!" other; Buy
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      Buy
  in

  let safe_order_type key f = 
    try match f (json |> member key) with
      | "Limit" -> Limit
      | "Market" -> Market
      | other -> Printf.printf "Unknown order_type '%s', defaulting to Limit\n%!" other; Limit
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      Limit
  in

  let safe_product key f = 
    try match f (json |> member key) with
      | "MIS" -> MIS
      | "CNC" -> CNC
      | "NRML" -> NRML
      | other -> Printf.printf "Unknown product '%s', defaulting to MIS\n%!" other; MIS
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      MIS
  in

  let safe_validity key f = 
    try match f (json |> member key) with
      | "DAY" -> DAY
      | "IOC" -> IOC
      | other -> Printf.printf "Unknown validity '%s', defaulting to DAY\n%!" other; DAY
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      DAY
  in

  {
    tradingsymbol = safe to_string "tradingsymbol";
    exchange = safe to_string "exchange";
    quantity = safe to_int "quantity";
    lot = safe to_int "quantity" / 75;
    price = safe to_float "price";
    trigger_price = safe to_float "trigger_price";
    side = safe_match "side" to_string;
    order_type = safe_order_type "order_type" to_string;
    product = safe_product "product" to_string;
    validity = safe_validity "validity" to_string;
    strategy_name = (try json |> member "strategy_name" |> to_string with _ -> "");
    status = None;
  }


let to_yojson (order : t) : Yojson.Safe.t =
  let string_of_side = function
    | Buy -> "Buy"
    | Sell -> "Sell"
  in

  let string_of_order_type = function
    | Limit -> "Limit"
    | Market -> "Market"
  in

  let string_of_product = function
    | MIS -> "MIS"
    | CNC -> "CNC"
    | NRML -> "NRML"
  in

  let string_of_validity = function
    | DAY -> "DAY"
    | IOC -> "IOC"
  in

  let base_fields = [
    "tradingsymbol", `String order.tradingsymbol;
    "exchange", `String order.exchange;
    "quantity", `Int order.quantity;
    "price", `Float order.price;
    "trigger_price", `Float order.trigger_price;
    "side", `String (string_of_side order.side);
    "lot", `Int order.lot;
    "order_type", `String (string_of_order_type order.order_type);
    "product", `String (string_of_product order.product);
    "validity", `String (string_of_validity order.validity);
  ] in

  let optional_fields =
    [ "status", Option.map (fun s -> `String (status_to_string s)) order.status ]
    |> List.filter_map (fun (k, v_opt) -> Option.map (fun v -> k, v) v_opt)
  in

  `Assoc (base_fields @ optional_fields)
