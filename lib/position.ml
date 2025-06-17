(* lib/position.ml *)
open Lwt
open Cohttp_lwt_unix

let ptime_to_yojson (t : Ptime.t) = `String (Ptime.to_rfc3339 t)
let ptime_of_yojson = function
  | `String s -> (match Ptime.of_rfc3339 s with
      | Ok (ptime, _, _) -> Ok ptime
      | Error _ -> Error "ptime_of_yojson: Invalid RFC3339 string")
  | _ -> Error "ptime_of_yojson: Expected string"

let option_to_yojson f = function
  | None -> `Null
  | Some x -> f x

let option_of_yojson f = function
  | `Null -> Ok None
  | json -> f json |> Result.map (fun x -> Some x)

let ptime_opt_to_yojson = option_to_yojson ptime_to_yojson
let ptime_opt_of_yojson = option_of_yojson ptime_of_yojson

type status =
  | Open
  | Closed[@@deriving yojson]

type side =
  | Buy
  | Sell[@@deriving yojson]

type pos = {
  opened_at : Ptime.t
    [@to_yojson ptime_to_yojson]
    [@of_yojson ptime_of_yojson];
  closed_at : Ptime.t option
    [@to_yojson ptime_opt_to_yojson]
    [@of_yojson ptime_opt_of_yojson];
  last_sell_time: Ptime.t option
    [@to_yojson ptime_opt_to_yojson]
    [@of_yojson ptime_opt_of_yojson];
  last_buy_time : Ptime.t option
    [@to_yojson ptime_opt_to_yojson]
    [@of_yojson ptime_opt_of_yojson];
  symbol : string;
  buy_qty : int;
  sell_qty: int;
  net_buy_price : float;
  net_sell_price : float;
  net_qty : int;
  net_price : float;
  side : side;
  value : float;
  status : status;
  pnl: float; (* already accumulated pnl from a previous closing *)
  delta : float;
  total_delta : float;
  vega : float;
  theta: float;
  gamma : float;
  rho : float;
}[@@deriving yojson]

let open_position_from_order (order : Order.t) : pos =
  let symbol = order.tradingsymbol in
  let qty = order.filled_quantity in
  let price = order.filled_price in (* since this represents the avg price that the quantity was filled at*)
  let side = order.side in
  Printf.printf "Adding new position for symbol %s with price %f and qty %d \n%!" symbol price qty;
  {
    opened_at = Ptime_clock.now ();
    closed_at = None;
    last_sell_time = (match side with | Buy -> None | Sell -> order.executed_at);
    last_buy_time = (match side with | Buy -> order.executed_at | Sell -> None);
    symbol;
    buy_qty = (match side with | Buy -> qty | Sell -> 0);
    sell_qty = (match side with | Buy -> 0 | Sell -> -qty);
    net_buy_price = (match side with | Buy -> price | Sell -> 0.0);
    net_sell_price = (match side with | Buy -> 0.0 | Sell -> price);
    side = (match side with | Buy -> Buy | Sell -> Sell);
    net_qty = (match side with | Buy -> qty | Sell -> -qty);
    net_price = price;
    value = (match side with | Buy -> float_of_int qty *. price | Sell -> -.float_of_int qty *. price);
    status = Open;
    pnl = 0.0;
    delta = 0.0;
    total_delta = 0.0;
    vega = 0.0;
    theta = 0.0;
    gamma = 0.0;
    rho = 0.0;
  }

let update_position_from_buy_order (pos : pos) (order : Order.t) : pos =
  let symbol = order.tradingsymbol in
  let qty = order.filled_quantity in
  let price = order.filled_price in (* since this represents the avg price that the quantity was filled at*)
  let now = Ptime_clock.now () in
  let total_qty = pos.net_qty + qty in
  let total_buy_cost = (float_of_int pos.buy_qty *. pos.net_buy_price) +. (float_of_int qty *. price) in
  let total_sell_cost = (float_of_int pos.sell_qty *. pos.net_sell_price) in
  let new_buy_price = total_buy_cost /. float_of_int (pos.buy_qty + qty) in
  let net_price, value, pnl, status, closed_at =
    if total_qty = 0 then
      (0.0, 0.0, -.(total_buy_cost +. total_sell_cost), Closed, Some now)
    else
      let net_price = (total_sell_cost +. total_buy_cost) /. float_of_int total_qty in
      (* keep on resetting the status to open because it may be followed by a Closed *)
      (net_price, float_of_int total_qty *. net_price, pos.pnl, Open, pos.closed_at) (* t2 + 15 for the close *)
  in
  let side = if total_qty > 0 then Buy else Sell in
  Printf.printf
    "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2d\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n\
             - pnl: %.2f\n%!"
    symbol
    price
    pos.net_price
    net_price
    total_qty
    "BUY"
    pos.value
    value
    pnl;
  { pos with
    buy_qty = pos.buy_qty + qty;
    net_qty = total_qty;
    net_buy_price = new_buy_price;
    net_price = net_price;
    value = value; 
    side = side;
    opened_at = now; (* lets us handle the loading case, where close should be on t2 + 15 *)
    last_buy_time = order.executed_at;
    pnl = pnl;
    status = status;
    closed_at = closed_at;
  }

let update_position_from_sell_order (pos : pos) (order : Order.t) : pos =
  let symbol = order.tradingsymbol in
  let qty = order.filled_quantity in
  let price = order.filled_price in (* since this represents the avg price that the quantity was filled at*)
  let now = Ptime_clock.now () in
  let total_qty = pos.net_qty - qty in
  let total_sell_cost = (float_of_int pos.sell_qty *. pos.net_sell_price) +. (-.float_of_int qty *. price) in
  let total_buy_cost = (float_of_int pos.buy_qty *. pos.net_buy_price) in
  let new_sell_price =  total_sell_cost /. float_of_int (pos.sell_qty - qty) in
  let net_price, value, pnl, status, closed_at=
    if total_qty = 0 then
      (0.0, 0.0, -.(total_buy_cost +. total_sell_cost), Closed, Some now)
    else
      let net_price = (total_sell_cost +. total_buy_cost) /. float_of_int total_qty in
      (net_price, float_of_int total_qty *. net_price, pos.pnl, Open, pos.closed_at)
  in
  let side = if total_qty > 0 then Buy else Sell in
  Printf.printf
    "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2d\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n\
             - pnl: %.2f\n%!"
    symbol
    price
    pos.net_price
    net_price
    total_qty
    "SELL"
    pos.value
    value
    pnl;
  { pos with
    sell_qty = pos.sell_qty - qty;
    net_qty = total_qty;
    net_sell_price = new_sell_price;
    net_price = net_price;
    value = value;
    side = side;
    opened_at = now;
    last_sell_time = order.executed_at;
    pnl = pnl;
    status = status;
    closed_at = closed_at;
  }

(* this is position over all "completed" orders for a particular symbol, not meant for partial orders. *)
let update_or_insert_position (positions : pos list) (order : Order.t) : pos list =
  let symbol = order.tradingsymbol in
  let side = order.side in
  let rec update_positions acc = function
    | [] ->
      let new_position = open_position_from_order order in
      List.rev (new_position :: acc)

    | pos :: rest when pos.symbol = symbol ->
      let updated_pos =
        match side with
        | Buy ->
          update_position_from_buy_order pos order

        | Sell ->
          update_position_from_sell_order pos order
      in
      List.rev_append acc (updated_pos :: rest)

    | pos :: rest ->
      (* if pos.symbol <> symbol then *)
      (*   Printf.printf "Mismatch: pos.symbol='%s' vs order.symbol='%s'\n%!" pos.symbol symbol; *)
      update_positions (pos :: acc) rest
  in

  update_positions [] positions

(*NOTE: only works for NIFTY15MAY23800PE*)
let extract_strike symbol =
  let len = String.length symbol in
  if len > 5 then
    String.sub symbol (len - 7) 7  (* "23800PE" is 7 characters *)
  else
    failwith ("Invalid symbol: " ^ symbol)

let find_option_data strike option_chain =
    let rec search = function
      | [] -> None
      | (_expiry, strikes) :: rest ->
        let rec find_in_strikes = function
          | [] -> search rest
          | (_strike_price, options) :: rest_opts ->
            match List.find_opt (fun (_key, (data : Option_chain.option_data)) -> data.strike = strike) options with
            | Some (_, data) -> Some data
            | None -> find_in_strikes rest_opts
        in
        find_in_strikes strikes
    in
    search option_chain

let send_position pos_str =
  let open Lwt.Syntax in
  let uri = Uri.of_string (Connector.get_env_or_default "REDIS_POSITION_URI" "https://steady-rabbit-13588.upstash.io/publish/positions_channel") in
  let pwd = Connector.get_env_or_default "REDIS_PWD" "abc" in
  let headers =
    Cohttp.Header.of_list [
      ("Authorization", "Bearer " ^ pwd);
      ("Content-Type", "application/json")
          ]
  in
  let body = Cohttp_lwt.Body.of_string pos_str in
  let* _, body = Client.post ~headers ~body uri in
  let* body_str = Cohttp_lwt.Body.to_string body in
  Printf.printf "Upstash response: %s\n%!" body_str;
  Lwt.return ()

let publish_positions (positions : pos list) : unit =
  List.iter (fun pos ->
    Printf.printf "Firing position to redis: %s\n%!" pos.symbol;
      let pos_str = pos_to_yojson pos |> Yojson.Safe.to_string in
      let _ : unit Lwt.t =
        send_position pos_str
        >>= fun (_) -> Lwt.return_unit
      (* Optionally add error logging here if you want *)
      in
      ()
      ) positions

let update_positions_with_option_chain
  (option_chain : Option_chain.t)
  (positions : pos list)
  : pos list =
  List.map
    (fun pos ->
      match find_option_data (extract_strike pos.symbol) option_chain with
      | Some data ->
        (* Printf.printf " found position symbol from option chain\n%!"; *)
        let value = float_of_int pos.net_qty *. data.ltp in
        Printf.printf "Updating position of symbol %s with option chain value from %f to %f and delta from %f to %f\n%!" pos.symbol pos.value value pos.delta data.delta;
        {
          pos with
          value;
          delta = data.delta *. 100.0;
          total_delta = data.delta *. 100.0 *. (float_of_int (abs pos.net_qty));
          vega = data.vega;
          theta = data.theta;
          gamma = data.gamma;
          rho = data.rho;
        }
      | None ->
        Printf.printf " NO position symbol found from option chain\n%!";
        (* Option data not found — return unchanged or log warning *)
        pos
    )
    positions
