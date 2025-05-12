(* bb_strategy.ml *)
open Strategy
open Unix

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

(* Event type specific to this strategy *)
type event =
  | Market_data_event of candle

(* Initialize the strategy state *)
let initial_local_state = {
  last_breach = Between;
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
      delta = delta;
      strike = "";
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

let lots_and_quantity (lot_size : int) (quantity : int) : int * int =
  let num_lots = quantity / lot_size in
  let adjusted_quantity = num_lots * lot_size in
  (num_lots, adjusted_quantity)

let generate_close_orders_for_position (option_chain : Option_chain.t) (pos : Position.t) : Order.t list =
  if pos.status = Closed then []
  else
    let quantity = - pos.net_qty 
    in
    let lots, adj_quantity = lots_and_quantity 75 quantity in
    let side =
      match pos.side with
      | Buy -> Order.Sell
      | Sell -> Order.Buy
    in
    let price = 
      match Position.find_option_data (Position.extract_strike pos.symbol) option_chain with
      | Some data -> data.ltp
      | None -> 0.
    in
    let order : Order.t = {
      tradingsymbol = pos.symbol;
      exchange = "NSE";
      quantity = abs adj_quantity; (*quantity in order is scalar, but in position it is a vector*)
      lot = lots;
      price;
      trigger_price = 0.0;
      side;
      order_type = Order.Market;
      product = Order.CNC;
      validity = Order.DAY;
      status = Some Order.Pending;
      strategy_name = "AutoClose";
    } in
    [order]

let expired_close_orders (positions : Position.t list) (option_chain: Option_chain.t) : Order.t list =
  positions
  |> List.filter (fun (pos : Position.t) -> pos.status = Open && match pos.strat_pos with
                                                                 | Position.Bb b -> b.candles == 1) (* (current_time -. pos.opened_at_epoch) >= 600.0 *)
  |> List.concat_map (generate_close_orders_for_position option_chain)

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

(* expiry like 08-05-2025 to unix epoch time *)
let expiry_to_epoch (date_str : string) : float =
  let day = int_of_string (String.sub date_str 0 2) in
  let month = int_of_string (String.sub date_str 3 2) in
  let year = int_of_string (String.sub date_str 6 4) in
  let tm = {
    Unix.tm_sec = 0;
    tm_min = 0;
    tm_hour = 0;
    tm_mday = day;
    tm_mon = month - 1;  (* months are 0-indexed *)
    tm_year = year - 1900;  (* years since 1900 *)
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false;
  } in
  fst (Unix.mktime tm)

let is_weekend tm =
  tm.tm_wday = 0 || tm.tm_wday = 6  (* Sunday or Saturday *)

let rec count_trading_days from_time to_time =
  if from_time > to_time then 0
  else
    let tm = Unix.localtime from_time in
    let next_day = from_time +. 86400.0 in (* add 1 day in seconds *)
    let rest = count_trading_days next_day to_time in
    if is_weekend tm then rest else 1 + rest

let get_offset_from_day (today : float) (expiry : float) : float =
  let trading_days = count_trading_days today expiry in
  match trading_days with
  | 4 -> 200.0
  | 3 -> 150.0
  | 2 -> 100.0
  | 1 -> 50.0
  | _ -> 0.0

let convert_date_to_symbol (date_str : string) : string =
  let month_abbr = [| ""; "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN";
    "JUL"; "AUG"; "SEP"; "OCT"; "NOV"; "DEC" |] in
  match String.split_on_char '-' date_str with
  | [day; month; _year] ->
    let month_num = int_of_string month in
    let abbr = month_abbr.(month_num) in
    day ^ abbr
  | _ -> failwith "Invalid date format"

let generate_upper_breach_orders ~option_chain ~candle ~offset : Order.t list =
  let expiry =
    match option_chain with
    | first:: _ -> fst first
    | [] -> ""
  in
  let current_price = candle.close_price in
  Printf.printf "current price is %f and offset %f\n%!" current_price offset;

  let call_strike = find_nearest_strike (current_price +. offset) option_chain in
  let call_data = get_option_data option_chain expiry call_strike "CE" in
  let call_qty = 7500 in
  let call_lots, call_adj_qty = lots_and_quantity 75 call_qty in
  let call_delta = abs_float call_data.delta in
  let call_delta_exposure = call_delta *. float_of_int call_adj_qty in

  let put_strike = find_nearest_strike (current_price -. offset) option_chain in
  let put_data = get_option_data option_chain expiry put_strike "PE" in
  let put_delta = abs_float put_data.delta in
  Printf.printf " call delta exposure and put delta are %f %f \n%!" call_delta_exposure put_delta;
  let put_qty = int_of_float (ceil (0.5 *. call_delta_exposure /. put_delta)) in
  let put_lots, put_adj_qty = lots_and_quantity 75 put_qty in
  let call_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ call_data.strike in
  let put_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ put_data.strike in

  let call_order =
    Order.make_order ~tradingsymbol:call_trading_symbol ~quantity:call_adj_qty ~lots: call_lots
                     ~price:call_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  let put_order =
    Order.make_order ~tradingsymbol:put_trading_symbol ~quantity:put_adj_qty ~lots:put_lots
                     ~price:put_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  [call_order; put_order]

let generate_lower_breach_orders ~option_chain ~candle ~offset : Order.t list =
  let expiry =
    match option_chain with
    | first:: _ -> fst first
    | [] -> ""
  in
  let current_price = candle.close_price in

  let put_strike = find_nearest_strike (current_price -. offset) option_chain in
  let put_data = get_option_data option_chain expiry put_strike "PE" in
  let put_qty = 7500 in
  let put_lots, put_adj_qty = lots_and_quantity 75 put_qty in
  let put_delta = abs_float put_data.delta in
  let put_delta_exposure = put_delta *. float_of_int put_adj_qty in

  let call_strike = find_nearest_strike (current_price +. offset) option_chain in
  let call_data = get_option_data option_chain expiry call_strike "CE" in
  let call_delta = abs_float call_data.delta in
  Printf.printf " put delta exposure and call delta are %f %f \n%!" put_delta_exposure call_delta;
  let call_qty = int_of_float (ceil (0.5 *. put_delta_exposure /. call_delta)) in
  let call_lots, call_adj_qty = lots_and_quantity 75 call_qty in
  let call_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ call_data.strike in
  let put_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ put_data.strike in

  let put_order =
    Order.make_order ~tradingsymbol:put_trading_symbol ~quantity:put_adj_qty ~lots:put_lots
                     ~price:put_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  let call_order =
    Order.make_order ~tradingsymbol:call_trading_symbol ~quantity:call_adj_qty ~lots:call_lots
                     ~price:call_data.ltp ~side:Order.Sell ~strategy_name:"bb"
  in
  [put_order; call_order]

(* Process the event and transform the state *)
let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state Lwt.t =
  (* Replace mock with actual async call to option chain *)
  match event with
  | Market_data_event candle ->

    let%lwt option_chain = Option_chain.get () in
    let current_time = Unix.gettimeofday () in

    (* Determine current breach status *)
    let current_breach =
      if candle.close_price > candle.upper_band then Upper
      else if candle.close_price < candle.lower_band then Lower
      else Between
    in
    let expiry =  (* Cornerstone: we are assuming the option_chain will always have the expiry to be worked upon *)
      match option_chain with
      | first:: _ -> fst first (*takes the expiry out of (expiry, strikes) pair *)
      | [] -> ""
    in
    (* let expiry = state.local_state.expiry in *)
    let expiry_epoch = expiry_to_epoch expiry in
    let offset = get_offset_from_day current_time expiry_epoch in

    (* Close positions if 10 minutes have passed *)
    let expired_close_orders = expired_close_orders state.positions option_chain in

    (* Orders based on breach transitions *)
    let transition_orders =
      match state.local_state.last_breach, current_breach with
      | Between, Upper ->
        Printf.printf "in Upper breach! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
        generate_upper_breach_orders ~option_chain ~candle ~offset

      | Between, Lower ->
        Printf.printf "in Lower breach! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
        generate_lower_breach_orders ~option_chain ~candle ~offset

      | Upper, Lower ->
        Printf.printf "in Upper Lower Zig Zag! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
        let close =
          state.positions
          |> List.concat_map (generate_close_orders_for_position option_chain) in
        let open_ = generate_lower_breach_orders ~option_chain ~candle ~offset in
        close @ open_

      | Lower, Upper ->
        Printf.printf "in Lower Upper Zig Zag! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
        let close =
          state.positions
          |> List.concat_map (generate_close_orders_for_position option_chain) in
        let open_ = generate_upper_breach_orders ~option_chain ~candle ~offset in
        close @ open_

      | Between, Between
        | _, Between
        | Upper, Upper
        | Lower, Lower -> 
        Printf.printf "NO breach! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
        generate_upper_breach_orders ~option_chain ~candle ~offset
    in

    let all_orders = expired_close_orders @ transition_orders @ state.pending_orders in
    let positions = Position.update_positions_with_option_chain option_chain state.positions in
    let new_local_state = { last_breach = current_breach } in
    let new_state = { state with pending_orders = all_orders; local_state = new_local_state;
                      positions = positions } in

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
