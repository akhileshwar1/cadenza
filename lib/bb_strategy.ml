(* bb_strategy.ml *)
open Strategy

type breach_status = 
  | Upper
  | Lower
  | Between 

(* Local state specific to AlternateStrategy *)
type local_state = {
  last_breach : breach_status;
  expiry : string;
} 

(* Config specific to AlternateStrategy *)
type local_config = unit

(* Define the candle type for this strategy *)
type candle = {
  timestamp : string;
  open_price : float;
  high_price : float;
  low_price : float;
  close_price : float;
  upper_band : float;
  lower_band : float;
  sma : float;
}

(* Event type specific to this strategy *)
type event =
  | Market_data_event of candle

(* Initialize the strategy state *)
let initial_local_state = {
  last_breach = Between;
  expiry = "2025-05-08";
}

(* Convert JSON to candle type *)
let json_to_candle (json : Yojson.Safe.t) : candle =
  let open Yojson.Safe.Util in
  {
    timestamp = json |> member "timestamp" |> to_string;
    open_price = json |> member "open" |> to_float;
    high_price = json |> member "high" |> to_float;
    low_price = json |> member "low" |> to_float;
    close_price = json |> member "close" |> to_float;
    upper_band = json |> member "upper_band" |> to_float;
    lower_band = json |> member "lower_band" |> to_float;
    sma = json |> member "sma" |> to_float;
  }


let generate_mock_option_chain candle : Option_chain.t Lwt.t =
  let spot = candle.close_price in
  let base_strike = Float.round (spot /. 100.0) *. 100.0 in
  let strikes = List.init 11 (fun i -> base_strike -. 500.0 +. (float_of_int (i * 100))) in

  let make_option_data strike call =
    let moneyness = abs_float (strike -. spot) in
    let delta = if call then
      max 0.0 (1.0 -. (moneyness /. 1000.0))
      else
        -. max 0.0 (1.0 -. (moneyness /. 1000.0)) in
    let premium = max 5.0 (100.0 -. moneyness /. 2.0) in
    let option: Option_chain.option_data = {
      symbol = "NIFTY50"; 
      ltp = premium;
      bid = premium -. 0.5;
      ask = premium +. 0.5;
      delta = delta;
    } in
    option
  in

  let expiry = "2025-05-08" in
  let options =
    strikes
    |> List.map (fun strike ->
      let call = make_option_data strike true in
      let put = make_option_data strike false in
      (strike, [("CE", call); ("PE", put)])
    )
  in

  let result : Option_chain.t = [(expiry, options)] in
  Lwt.return result


(* Convert JSON to event *)
let json_to_event (json : Yojson.Safe.t) : event =
  let candle = json_to_candle json in
  Market_data_event candle

let generate_close_orders_for_position (pos : Position.t) : Order.t list =
  if pos.status = Closed then []
  else
    let quantity =
      match pos.side with
      | Buy -> pos.net_buy_qty
      | Sell -> pos.net_sell_qty
    in
    let side =
      match pos.side with
      | Buy -> Order.Sell
      | Sell -> Order.Buy
    in
    let price =
      match side with
      | Order.Buy -> pos.current_ask_price
      | Order.Sell -> pos.current_bid_price
    in
    let order : Order.t = {
      tradingsymbol = pos.symbol;
      exchange = "NSE";
      quantity;
      price;
      trigger_price = 0.0;
      side;
      order_type = Order.Market;
      product = Order.MIS;
      validity = Order.DAY;
      status = Some Order.Pending;
      strategy_name = "AutoClose";
    } in
    [order]

let expired_close_orders (positions : Position.t list) (current_time : float) : Order.t list =
  positions
  |> List.filter (fun (pos : Position.t) -> pos.status = Open && (current_time -. pos.opened_at_epoch) >= 600.0)
  |> List.concat_map generate_close_orders_for_position

let find_nearest_strike (target : float) (option_chain : Option_chain.t) : float =
  let all_strikes =
    option_chain
    |> List.hd |> snd (* pick any expiry, doesn't matter *)
    |> List.map fst
  in
  List.fold_left (fun acc strike ->
    if abs_float (strike -. target) < abs_float (acc -. target) then strike else acc
  ) (List.hd all_strikes) all_strikes

let get_option_data (option_chain : Option_chain.t) (expiry : string) (strike : float) (otype : string) : Option_chain.option_data =
  match List.assoc_opt expiry option_chain with
  | Some strike_map ->
    (match List.assoc_opt strike strike_map with
      | Some data_map ->
        List.assoc otype data_map
      | None -> failwith "Strike not found")
  | None -> failwith "Expiry not found"


let get_offset_from_day (epoch_time : float) : float =
  let tm = Unix.localtime epoch_time in
  match tm.tm_wday with
  | 1 -> 200.0 
  | 2 -> 150.0
  | 3 -> 100.0
  | 4 -> 50.0
  | _ -> 0.0

let generate_upper_breach_orders ~state ~option_chain ~candle ~offset : Order.t list =
  let expiry = state.local_state.expiry in
  let current_price = candle.close_price in

  let call_strike = find_nearest_strike (current_price +. offset) option_chain in
  let call_data = get_option_data option_chain expiry call_strike "CE" in
  let call_qty = 10000 in
  let call_delta = abs_float call_data.delta in
  let call_delta_exposure = call_delta *. float_of_int call_qty in

  let put_strike = find_nearest_strike (current_price -. offset) option_chain in
  let put_data = get_option_data option_chain expiry put_strike "PE" in
  let put_delta = abs_float put_data.delta in
  let put_qty = int_of_float (ceil (0.5 *. call_delta_exposure /. put_delta)) in

  let call_order =
    Order.make_order ~tradingsymbol:call_data.symbol ~quantity:call_qty ~price:call_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  let put_order =
    Order.make_order ~tradingsymbol:put_data.symbol ~quantity:put_qty ~price:put_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  [call_order; put_order]

let generate_lower_breach_orders ~state ~option_chain ~candle ~offset : Order.t list =
  let expiry = state.local_state.expiry in
  let current_price = candle.close_price in

  let put_strike = find_nearest_strike (current_price -. offset) option_chain in
  let put_data = get_option_data option_chain expiry put_strike "PE" in
  let put_qty = 10000 in
  let put_delta = abs_float put_data.delta in
  let put_delta_exposure = put_delta *. float_of_int put_qty in

  let call_strike = find_nearest_strike (current_price +. offset) option_chain in
  let call_data = get_option_data option_chain expiry call_strike "CE" in
  let call_delta = abs_float call_data.delta in
  let call_qty = int_of_float (ceil (0.5 *. put_delta_exposure /. call_delta)) in

  let put_order =
    Order.make_order ~tradingsymbol:put_data.symbol ~quantity:put_qty ~price:put_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  let call_order =
    Order.make_order ~tradingsymbol:call_data.symbol ~quantity:call_qty ~price:call_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  [put_order; call_order]

(* Process the event and transform the state *)
let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state Lwt.t =
  (* Replace mock with actual async call to option chain *)
  match event with
  | Market_data_event candle ->

    let%lwt option_chain = generate_mock_option_chain candle in
    let current_time = Unix.gettimeofday () in

    (* Determine current breach status *)
    let current_breach =
      if candle.close_price > candle.upper_band then Upper
      else if candle.close_price < candle.lower_band then Lower
      else Between
    in

    let offset = get_offset_from_day current_time in

    (* Close positions if 10 minutes have passed *)
    let expired_close_orders = expired_close_orders state.positions current_time in

    (* Orders based on breach transitions *)
    let transition_orders =
      match state.local_state.last_breach, current_breach with
      | Between, Upper ->
        Printf.printf "in upper breach! %!";
        generate_upper_breach_orders ~state ~option_chain ~candle ~offset
      | Between, Lower ->
        Printf.printf "in lower breach! %!";
        generate_lower_breach_orders ~state ~option_chain ~candle ~offset

      | Upper, Lower ->
        let close =
          state.positions
          |> List.concat_map generate_close_orders_for_position in
        let open_ = generate_lower_breach_orders ~state ~option_chain ~candle ~offset in
        close @ open_

      | Lower, Upper ->
        let close =
          state.positions
          |> List.concat_map generate_close_orders_for_position in
        let open_ = generate_upper_breach_orders ~state ~option_chain ~candle ~offset in
        close @ open_

      | Between, Between
        | _, Between
        | Upper, Upper
        | Lower, Lower -> []
    in

    let all_orders = expired_close_orders @ transition_orders @ state.pending_orders in
    let new_local_state = { state.local_state with last_breach = current_breach } in
    let new_state = { state with pending_orders = all_orders; local_state = new_local_state } in

    Lwt.return new_state

let extract_orders (state : 'local_state Strategy.state) : Order.t list * 'local_state Strategy.state =
  let orders_to_extract = state.pending_orders in
  let new_state = { state with pending_orders = [] } in (* Create a new state with pending_orders cleared *)
  (orders_to_extract, new_state) (* Return the orders and the new state *)

(* The final strategy packaged together *)
let create (config : ('local_config, 'local_state) Strategy.config) : ('local_config, 'local_state) Strategy.t =  Strategy.create
  config
  initial_local_state
  extract_orders
