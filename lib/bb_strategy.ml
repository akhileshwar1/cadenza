(* alternate_strategy.ml *)

open Strategy

type breach_status = 
  | Upper
  | Lower
  | Between 

(* Local state specific to AlternateStrategy *)
type local_state = {
  last_breach : breach_status;
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

type option_data = {
  symbol : string;
  premium : float;
  delta : float;
}

type option_chain = (string * (float * (string * option_data) list) list) list

(* Event type specific to this strategy *)
type event =
  | Market_data_event of candle

(* Initialize the strategy state *)
let initial_local_state = () 

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
  |> List.filter (fun pos ->
         pos.status = Open && (current_time -. pos.opened_at_epoch) >= 600.0)
  |> List.concat_map generate_close_orders_for_position

let find_nearest_strike (target : float) (option_chain : option_chain) : float =
  let all_strikes =
    option_chain
    |> List.hd |> snd (* pick any expiry, doesn't matter *)
    |> List.map fst
  in
  List.fold_left (fun acc strike ->
    if abs_float (strike -. target) < abs_float (acc -. target) then strike else acc
  ) (List.hd all_strikes) all_strikes

let get_option_data (option_chain : option_chain) (expiry : string) (strike : float) (otype : string) : option_data =
  match List.assoc_opt expiry option_chain with
  | Some strike_map ->
    (match List.assoc_opt strike strike_map with
      | Some data_map ->
        let data = List.assoc otype data_map in
        {
          symbol = data.symbol;
          premium = data.ltp;
          delta = data.delta;
        }
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
  let expiry = state.expiry in
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
    Order.make_order ~symbol:call_data.symbol ~qty:call_qty ~price:call_data.premium ~side:Order.Sell ~strategy_name:"bb"
  in
  let put_order =
    
    Order.make_order ~symbol:put_data.symbol ~qty:put_qty ~price:put_data.premium ~side:Order.Sell ~strategy_name:"bb"
  in
  [call_order; put_order]

let generate_lower_breach_orders ~state ~option_chain ~candle ~offset : Order.t list =
  let expiry = state.expiry in
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
    Order.make_order ~symbol:put_data.symbol ~qty:put_qty ~price:put_data.premium ~side:Order.Sell ~strategy_name:"bb"
  in
  let call_order =
    Order.make_order ~symbol:call_data.symbol ~qty:call_qty ~price:call_data.premium ~side:Order.Sell ~strategy_name:"bb"
  in
  [put_order; call_order]

(* Process the event and transform the state *)
let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state =
  (* Mocked option chain — replace with actual call in production *)
  let option_chain = MockOptionChain.get () in

  match event with
  | Market_data_event candle ->
      let current_time = Unix.gettimeofday () in

      (* Determine current breach status *)
      let current_breach =
        if candle.close_price > candle.upper_band then Upper
        else if candle.close_price < candle.lower_band then Lower
        else Between
      in
      let current_time = Unix.gettimeofday () in
      let offset = get_offset_from_day current_time in
      (* Close positions if 10 minutes have passed *)
      let expired_close_orders = expired_close_orders state.positions current_time in
      (* Orders based on breach transitions *)
      let transition_orders =
        match state.local_state.last_breach, current_breach with
        | Between, Upper ->
            generate_upper_breach_orders ~state ~option_chain ~candle
        | Between, Lower ->
            generate_lower_breach_orders ~state ~option_chain ~candle
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
        | _, Between | Between, Between -> []
        | Upper, Upper
        | Lower, Lower -> []  (* Continue in same breach — no new action *)
      in

      let all_orders = expired_close_orders @ transition_orders in
      Strategy.update_state_with_orders state ~orders:all_orders ~breach_status:current_breach ~timestamp:candle.timestamp

let extract_orders (state : 'local_state Strategy.state) : Order.t list * 'local_state Strategy.state =
  let orders_to_extract = state.pending_orders in
  let new_state = { state with pending_orders = [] } in (* Create a new state with pending_orders cleared *)
  (orders_to_extract, new_state) (* Return the orders and the new state *)

(* The final strategy packaged together *)
let create (config : ('local_config, 'local_state) Strategy.config) : ('local_config, 'local_state) Strategy.t =  Strategy.create
    config
    initial_local_state
    extract_orders
