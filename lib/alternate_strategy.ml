(* alternate_strategy.ml *)

open Strategy

(* Local state specific to AlternateStrategy *)
type local_state = {
  last_side : Order.side option;
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
}

(* Event type specific to this strategy *)
type event =
  | Market_data_event of candle
  | Timer_tick_event of float

(* Initialize the strategy state *)
let initial_local_state = {
  last_side = None;
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
  }

(* Convert JSON to event *)
let json_to_event (json : Yojson.Safe.t) : event =
  let candle = json_to_candle json in
  Market_data_event candle

(* Process the event and transform the state *)
let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state =
  match event with
  | Market_data_event candle ->
    (* Handle candle data and determine buy/sell logic based on alternating candles *)
    let next_side =
      match state.local_state.last_side with
      | None -> Order.Buy          (* Start with Buy *)
      | Some Order.Buy -> Order.Sell
      | Some Order.Sell -> Order.Buy
    in

    (* Create the new order based on the candle data *)
    let new_order = {
      Order.tradingsymbol = "TCS";  (* Hardcoded for now *)
      exchange = "NSE";
      quantity = 50;
      lot = 0;
      price = candle.close_price;  (* Market order -> fill at close price *)
      trigger_price = 0.0;
      side = next_side;
      order_type = Order.Market;
      product = Order.MIS;
      validity = Order.DAY;
      status = Some Order.Pending;
      strategy_name = "Glitters_alternate"
    } in

    (* Update the state with new order *)
    {
      state with
      pending_orders = new_order :: state.pending_orders;
      local_state = { last_side = Some next_side };  (* Flip side after each order *)
    }
  | Timer_tick_event _ -> 
    (* You can implement timer-based logic if needed in the future *)
    state

let extract_orders (state : 'local_state Strategy.state) : Order.t list * 'local_state Strategy.state =
  let orders_to_extract = state.pending_orders in
  let new_state = { state with pending_orders = [] } in (* Create a new state with pending_orders cleared *)
  (orders_to_extract, new_state) (* Return the orders and the new state *)

(* The final strategy packaged together *)
let create (config : ('local_config, 'local_state) Strategy.config) : ('local_config, 'local_state) Strategy.t =  Strategy.create
    config
    initial_local_state
    extract_orders
