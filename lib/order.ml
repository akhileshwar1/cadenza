(* lib/order.ml *)

(* Basic Order Types *)

let uuid = Uuidm.v4_gen (Random.State.make_self_init ())
let generate_order_id () : string =
  Uuidm.to_string (uuid ())

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

let string_to_status = function
  | "Cancelled" -> Cancelled
  | "Pending" -> Pending
  | "Rejected" -> Rejected
  | "Completed" -> Completed
  | "Partially Executed" -> Partial
  | _ -> Unknown

let status_to_string = function
  | Cancelled -> "Cancelled"
  | Pending -> "Pending"
  | Rejected -> "Rejected"
  | Completed -> "Completed"
  | Partial -> "Partially Executed"
  | Created -> "Created"
  | Unknown -> "Unknown"

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

(* Helper function to convert an Order.t to a Yojson.Safe.t, caled while sending to OMS. *)
let json_of_order (order : t) : Yojson.Safe.t =
  `Assoc [
    ("placed_at", 
      match order.placed_at with
      | Some ts -> `String (Ptime.to_rfc3339 ts)
      | None -> `Null);
    ("executed_at", 
      match order.executed_at with
      | Some ts -> `String (Ptime.to_rfc3339 ts)
      | None -> `Null);
    ("tradingsymbol", `String order.tradingsymbol);
    ("side", `String (match order.side with | Buy -> "Buy" | Sell -> "Sell"));
    ("validity", `String (match order.validity with | DAY -> "DAY" | IOC -> "IOC"));
    ("product", `String (match order.product with | MIS -> "MIS" | CNC -> "CNC" | NRML -> "NRML"));
    ("status", `String (match order.status with
      | Some Completed -> "Completed"
      | Some Pending -> "Pending" 
      | Some Rejected -> "Rejected"
      | Some _ -> "Unknown"
      | None -> "Pending"));
    ("quantity", `Int order.quantity);
    ("filled_quantity", `Int order.filled_quantity);
    ("lot", `Int order.lot);
    ("price", `Float order.price);
    ("filled_price", `Float order.filled_price);
    ("trigger_price", `Float order.trigger_price);
    ("order_type", `String (match order.order_type with | Limit -> "Limit" | Market -> "Market"));
    ("exchange", `String order.exchange);
    ("strategy_name", `String order.strategy_name);
    ("order_id", `String  order.order_id);
    ("broker_order_id", `String  order.broker_order_id)
  ]

let freeze_qty = 1800

let split_quantity quantity =
  let rec aux remaining acc =
    if remaining <= 0 then List.rev acc
    else
      let this_qty = min freeze_qty remaining in
      aux (remaining - this_qty) (this_qty :: acc)
  in
  aux quantity []

let make_order
  ~(tradingsymbol : string)
  ~(quantity : int)
  ~(lots : int)
  ~(price : float)
  ~(side : side)
  ~(strategy_name : string)
  : t list =
  let chunks = split_quantity quantity in
  List.map
    (fun q ->
      {
        placed_at = Some (Ptime_clock.now ());
        executed_at = None;
        tradingsymbol;
        exchange = "NSE";
        quantity = q;
        price;
        trigger_price = 0.0;
        side;
        order_type = Market;
        product = CNC;
        validity = DAY;
        status = Some Pending;
        lot = lots;
        strategy_name;
        filled_quantity = 0;
        filled_price = 0.0;
        broker_order_id = "";
        order_id = generate_order_id ();
      }
    )
  chunks


let ptime_of_string (s : string) : Ptime.t option =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ -> None

(* called for order update*)
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

  (* let safe_product key f =  *)
  (*   try match f (json |> member key) with *)
  (*     | "MIS" -> MIS *)
  (*     | "CNC" -> CNC *)
  (*     | "NRML" -> NRML *)
  (*     | other -> Printf.printf "Unknown product '%s', defaulting to MIS\n%!" other; MIS *)
  (*   with e -> *)
  (*     Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e); *)
  (*     MIS *)
  (* in *)

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
    placed_at = None; (* will be None, since order update has no memory of
                                                                 what happened, directly relayed from broker *)
    executed_at  = ptime_of_string (safe to_string "executed_at");
    tradingsymbol = safe to_string "tradingsymbol";
    exchange = safe to_string "exchange";
    quantity = safe to_int "quantity";
    filled_quantity = safe to_int "filled_quantity";
    lot = safe to_int "lot";
    price = safe to_float "price";
    filled_price = safe to_float "filled_price";
    trigger_price = safe to_float "trigger_price";
    side = safe_match "side" to_string;
    order_type = safe_order_type "order_type" to_string;
    product = CNC;
    validity = safe_validity "validity" to_string;
    strategy_name = (try json |> member "strategy_name" |> to_string with _ -> "");
    status = Some (string_to_status (safe to_string "status"));
    order_id = safe to_string "order_id";
    broker_order_id = safe to_string "broker_order_id";
  }


(* let to_yojson (order : t) : Yojson.Safe.t = *)
(*   let string_of_side = function *)
(*     | Buy -> "Buy" *)
(*     | Sell -> "Sell" *)
(*   in *)

(*   let string_of_order_type = function *)
(*     | Limit -> "Limit" *)
(*     | Market -> "Market" *)
(*   in *)

(*   let string_of_product = function *)
(*     | MIS -> "MIS" *)
(*     | CNC -> "CNC" *)
(*     | NRML -> "NRML" *)
(*   in *)

(*   let string_of_validity = function *)
(*     | DAY -> "DAY" *)
(*     | IOC -> "IOC" *)
(*   in *)

(*   let base_fields = [ *)
(*     "tradingsymbol", `String order.tradingsymbol; *)
(*     "exchange", `String order.exchange; *)
(*     "quantity", `Int order.quantity; *)
(*     "price", `Float order.price; *)
(*     "trigger_price", `Float order.trigger_price; *)
(*     "side", `String (string_of_side order.side); *)
(*     "lot", `Int order.lot; *)
(*     "order_type", `String (string_of_order_type order.order_type); *)
(*     "product", `String (string_of_product order.product); *)
(*     "validity", `String (string_of_validity order.validity); *)
(*     "order_id", `String order.order_id; *)
(*     "broker_order_id" , `String order.broker_order_id *)
(*   ] in *)

(*   let optional_fields = *)
(*     [ "status", Option.map (fun s -> `String (status_to_string s)) order.status ] *)
(*     |> List.filter_map (fun (k, v_opt) -> Option.map (fun v -> k, v) v_opt) *)
(*   in *)

(*   `Assoc (base_fields @ optional_fields) *)

let total_cost order1 order2 =
  (((float_of_int order1.filled_quantity) *. order1.filled_price) +. ((float_of_int order2.filled_quantity) *. order2.filled_price))

let total_quantity order1 order2 =
  ((float_of_int order1.filled_quantity) +. (float_of_int order2.filled_quantity))

let update_filled_price_and_quantity order1 order2 =
  let updated_quantity = total_quantity order1 order2 in
  let updated_filled_price = total_cost order1 order2 /. updated_quantity in
  (updated_filled_price, int_of_float updated_quantity)

let apply_order_update order_update order =
  if order.broker_order_id = order_update.broker_order_id then
    let updated_filled_price, updated_quantity = update_filled_price_and_quantity order order_update in
    {order with 
      filled_quantity = updated_quantity;
      filled_price = updated_filled_price;
      status = order_update.status;
      executed_at = order_update.executed_at;
      broker_order_id = order_update.broker_order_id}
  else
    order
