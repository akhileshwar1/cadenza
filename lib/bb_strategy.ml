(* bb_strategy.ml *)
(* Let this module be purely functional always *)
open Strategy
open Lwt.Infix

type breach_status = 
  | Upper
  | Lower
  | Between 

(* Define the candle type for this strategy *)
type candle = {
  timestamp : Ptime.t;
  open_price : float;
  high_price : float;
  low_price : float;
  close_price : float;
  upper_band : float;
  lower_band : float;
  sma : float;
}

(* Local state specific to AlternateStrategy *)
type local_state = {
  last_breach : breach_status;
  candle : candle;
  option_chain : Option_chain.t;
  lots_sold_for_current_candle : int;
  candle_lots_limit : int;
} 

(* Config specific to AlternateStrategy *)
type local_config = unit

(* Event type specific to this strategy *)
type event =
  | Market_data_event of candle

(* Initialize the strategy state *)
let initial_local_state = {
  last_breach = Between;
  candle = {timestamp = Ptime_clock.now (); open_price = 0.0; high_price = 0.0; low_price = 0.0;
            close_price = 0.0; upper_band = 0.0; lower_band = 0.0; sma = 0.0};
  option_chain = [];
  lots_sold_for_current_candle = 0;
  candle_lots_limit = 10;
}

let write_header_to_csv (file : string) =
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 file in
  Printf.fprintf oc "%s,%s,%s,%s,%s,%s,%s,%s\n"
    "timestamp"
    "symbol"
    "net_buy_price"
    "buy_qty"
    "net_sell_price"
    "sell_qty"
    "value"
    "pnl";
  close_out oc

let write_position_to_csv (file : string) (pos : Position.t) =
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 file in
  let (year, month, day), ((hour, min, sec), _) = Ptime.to_date_time pos.opened_at in
  let timestamp =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      year month day hour min sec
  in
  Printf.fprintf oc "%s,%s,%.2f,%d,%.2f,%d,%.2f,%.2f\n"
    timestamp
    pos.symbol
    pos.net_buy_price
    pos.buy_qty
    pos.net_sell_price
    pos.sell_qty
    pos.value
    pos.pnl;
  close_out oc

(* Convert JSON to candle type *)
(*"timestamp": "2025-05-28T12:30:00+04:00"*)
let json_to_candle (json : Yojson.Safe.t) : candle =
  let open Yojson.Safe.Util in
  let timestamp_str = json |> member "timestamp" |> to_string in
  match Ptime.of_rfc3339 timestamp_str with
  | Ok (ptime, _, _) ->
      {
        timestamp = ptime;
        open_price = json |> member "open" |> to_float;
        high_price = json |> member "high" |> to_float;
        low_price = json |> member "low" |> to_float;
        close_price = json |> member "close" |> to_float;
        upper_band = json |> member "upper_band" |> to_float;
        lower_band = json |> member "lower_band" |> to_float;
        sma = json |> member "sma" |> to_float;
     }
  | Error _ ->
      failwith ("Invalid timestamp format: " ^ timestamp_str)

(* candle timestamps aren't delayed now *)
let is_outside_trading_window (timestamp : Ptime.t) : bool =
  match Ptime.to_date_time timestamp with
  | ((_, _, _), ((hour, min, _), _)) ->
    let minutes = hour * 60 + min in
    minutes < (4 * 60) || minutes > (9 * 60 + 35) (* first posn at 8:00 am dst and last at 1:35pm dst *)

let is_time (timestamp : Ptime.t) (mins_time : int) : bool =
  match Ptime.to_date_time timestamp with
  | (_, ((hour, min, _), _)) ->
    let minutes = hour * 60 + min in
    minutes = mins_time
  (* (13 * 60 + 55) (* 3:25 pm IST *) *)

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
      timestamp = Ptime_clock.now ();
      symbol = "NIFTY50"; 
      ltp = premium;
      delta = delta;
      strike = "";
      iv = 0.0;
      vega = 0.0;
      theta = 0.0;
      gamma = 0.0;
      rho = 0.0;
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
    let order : Order.t = 
      Order.make_order 
        ~tradingsymbol:pos.symbol
        ~quantity:(abs adj_quantity)
        ~lots:lots 
        ~price:price
        ~side:side
        ~strategy_name:"bb"
    in
    [order]

(* close orders after 15 minutes/on the 3rd candle *)
let expired_close_orders (current_time : Ptime.t) (positions : Position.t list) (option_chain: Option_chain.t) : Order.t list =
  positions
  |> List.filter (fun (pos : Position.t) ->
    (pos.status = Open &&
      match pos.last_sell_time with
      | Some sell_time ->
        let diff = Ptime.diff current_time sell_time in
        let minutes = int_of_float (Ptime.Span.to_float_s diff /. 60.0) in
        minutes >= 20
      | None -> false)) (* (current_time -. pos.opened_at_epoch) >= 600.0 *)
  |> List.concat_map (generate_close_orders_for_position option_chain)

(* used in cases where you want to make sure you are not carrying a position overnight *)
let close_all_open_orders (positions : Position.t list) (option_chain: Option_chain.t) : Order.t list =
  positions
  |> List.filter (fun (pos : Position.t) -> pos.status = Open) (* (current_time -. pos.opened_at_epoch) >= 600.0 *)
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

(* expiry like "08-05-2025" to Ptime.t *)
let expiry_to_ptime (date_str : string) : Ptime.t option =
  let day = int_of_string (String.sub date_str 0 2) in
  let month = int_of_string (String.sub date_str 3 2) in
  let year = int_of_string (String.sub date_str 6 4) in
  (* Set time to 00:00:00 *)
  Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0))

let weekday_of_ptime (pt : Ptime.t) : int =
  match Ptime.to_float_s pt with
  | float_ts ->
    let unix_tm = Unix.gmtime float_ts in
    (* tm_wday: 0 = Sunday, 1 = Monday, ..., 6 = Saturday *)
    unix_tm.Unix.tm_wday

let is_weekend (pt : Ptime.t) : bool =
  match Ptime.to_float_s pt with
  | float_ts ->
    let weekday = (Unix.gmtime float_ts).Unix.tm_wday in
    (* Printf.printf "weekday for %s is %d\n%!" (Ptime.to_rfc3339 pt) weekday; *)
    weekday = 0 || weekday = 6  (* Sunday or Saturday *)

(* what about the edge case where both the times are on the same day? *)
let rec count_trading_days (from_time : Ptime.t) (to_time : Ptime.t) : int =
  if Ptime.is_later ~than:to_time from_time then 0
  else
    match Ptime.Span.of_d_ps (1, 0L) with
    | Some one_day -> (
      match Ptime.add_span from_time one_day with
      | Some next_day ->
        let rest = count_trading_days next_day to_time in
        if is_weekend from_time then rest else 1 + rest
      | None -> 0)
    | None -> 0

(* assumes that it wouldn't be called in the case where we are on the expiry day because our option_chain.get
   handles that *)
let get_offset_from_day (today : Ptime.t) (expiry : Ptime.t) : float =
  let trading_days = count_trading_days today expiry in
  Printf.printf "trading days is %d\n" trading_days;
  match trading_days with
  | 5 -> 250.0
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

(* Check if the symbol has a sell within the last 10 seconds *)
let has_recent_sell ~positions ~symbol ~now : bool =
  List.exists (fun pos ->
    pos.symbol = symbol &&
    match pos.last_sell_time with
    | Some last_time ->
      Ptime.Span.compare (Ptime.diff now last_time) (Ptime.Span.of_int_s 10) < 0
    | None -> false
  ) positions

(* Generate order only if no recent sell for the symbol *)
let generate_if_not_recently_sold ~symbol ~qty ~lots ~price ~side ~positions ~now : Order.t list =
  if has_recent_sell ~positions ~symbol ~now then (
    Printf.printf "Skipping order for %s: recently sold.\n%!" symbol;
    []
  ) else
    [Order.make_order
      ~tradingsymbol:symbol
      ~quantity:qty
      ~lots: lots
      ~price: price
      ~side: side
      ~strategy_name:"bb"]

let generate_upper_breach_orders ~option_chain ~candle ~offset ~positions : Order.t list =
  let expiry =
    match option_chain with
    | first:: _ -> fst first
    | [] -> ""
  in
  let current_price = candle.close_price in
  let now = candle.timestamp in
  Printf.printf "current price is %f and offset %f\n%!" current_price offset;

  let call_strike = find_nearest_strike (current_price +. offset) option_chain in
  let call_data = get_option_data option_chain expiry call_strike "CE" in
  let call_qty = 750 in
  let call_lots, call_adj_qty = lots_and_quantity 75 call_qty in
  let call_delta = abs_float call_data.delta in
  let call_delta_exposure = call_delta *. float_of_int call_adj_qty in

  let put_strike = find_nearest_strike (current_price -. offset) option_chain in
  let put_data = get_option_data option_chain expiry put_strike "PE" in
  let put_delta = abs_float put_data.delta in
  Printf.printf " call delta exposure and put delta are %f %f \n%!" call_delta_exposure put_delta;
  let put_qty = int_of_float (ceil (0.75 *. call_delta_exposure /. put_delta)) in
  let put_lots, put_adj_qty = lots_and_quantity 75 put_qty in
  let call_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ call_data.strike in
  let put_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ put_data.strike in
  let call_orders =
    generate_if_not_recently_sold
      ~symbol:call_trading_symbol
      ~qty:call_adj_qty
      ~lots:call_lots
      ~price:call_data.ltp
      ~side:Order.Sell
      ~positions
      ~now
  in
  let put_orders =
    generate_if_not_recently_sold
      ~symbol:put_trading_symbol
      ~qty:put_adj_qty
      ~lots:put_lots
      ~price:put_data.ltp
      ~side:Order.Sell
      ~positions
      ~now
  in
  call_orders @ put_orders
 
  
let generate_lower_breach_orders ~option_chain ~candle ~offset ~positions : Order.t list =
  let expiry =
    match option_chain with
    | first:: _ -> fst first
    | [] -> ""
  in
  let current_price = candle.close_price in
  let now = candle.timestamp in

  let put_strike = find_nearest_strike (current_price -. offset) option_chain in
  let put_data = get_option_data option_chain expiry put_strike "PE" in
  let put_qty = 750 in
  let put_lots, put_adj_qty = lots_and_quantity 75 put_qty in
  let put_delta = abs_float put_data.delta in
  let put_delta_exposure = put_delta *. float_of_int put_adj_qty in

  let call_strike = find_nearest_strike (current_price +. offset) option_chain in
  let call_data = get_option_data option_chain expiry call_strike "CE" in
  let call_delta = abs_float call_data.delta in
  Printf.printf " put delta exposure and call delta are %f %f \n%!" put_delta_exposure call_delta;
  let call_qty = int_of_float (ceil (0.75 *. put_delta_exposure /. call_delta)) in
  let call_lots, call_adj_qty = lots_and_quantity 75 call_qty in
  let call_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ call_data.strike in
  let put_trading_symbol = "NIFTY" ^ convert_date_to_symbol expiry ^ put_data.strike in

  let call_orders =
    generate_if_not_recently_sold
      ~symbol:call_trading_symbol
      ~qty:call_adj_qty
      ~lots:call_lots
      ~price:call_data.ltp
      ~side:Order.Sell
      ~positions
      ~now
  in
  let put_orders =
    generate_if_not_recently_sold
      ~symbol:put_trading_symbol
      ~qty:put_adj_qty
      ~lots:put_lots
      ~price:put_data.ltp
      ~side:Order.Sell
      ~positions
      ~now
  in
  call_orders @ put_orders


let transition_orders ~current_breach ~last_breach ~candle ~option_chain ~offset ~positions =
  if is_outside_trading_window candle.timestamp then
    []
  else
    (* Orders based on breach transitions and persistence.*)
    match last_breach, current_breach with
    | Between, Upper
      | Upper, Upper
      ->
      Printf.printf "in Upper breach! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
      generate_upper_breach_orders ~option_chain ~candle ~offset ~positions

    | Between, Lower
      | Lower, Lower
      ->
      Printf.printf "in Lower breach! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
      generate_lower_breach_orders ~option_chain ~candle ~offset ~positions

    | Upper, Lower -> (* Note: we will have to write a case where opposite breach happens within 20 mins, this isn't correct. *)
      Printf.printf "in Upper Lower Zig Zag! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
      let close =
        positions
        |> List.concat_map (generate_close_orders_for_position option_chain) in
      let open_ = generate_lower_breach_orders ~option_chain ~candle ~offset ~positions in
      close @ open_

    | Lower, Upper ->
      Printf.printf "in Lower Upper Zig Zag! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
      let close =
        positions
        |> List.concat_map (generate_close_orders_for_position option_chain) in
      let open_ = generate_upper_breach_orders ~option_chain ~candle ~offset ~positions in
      close @ open_

    | Between, Between
      | _, Between -> 
      Printf.printf "NO breach! %! %f %f %f \n %!" candle.lower_band candle.close_price candle.upper_band;
      []
      (* generate_upper_breach_orders ~option_chain ~candle ~offset *)

(* close all open orders, don't want no open positions into the night, for the night is dark *)
(* let close_time_orders ~candle ~positions ~option_chain = *)
(*   if is_time candle.timestamp (13 * 60 + 50) then (* 5 mins before day close *) *)
(*     close_all_open_orders positions option_chain *)
(*   else *)
(*     [] *)

let get_breach_type ~candle =
  if candle.close_price > candle.upper_band then Upper
  else if candle.close_price < candle.lower_band then Lower
  else Between

let current_expiry_from_option_chain ~option_chain =
  match option_chain with
  | first:: _ -> fst first (*takes the expiry out of (expiry, strikes) pair *)
  | [] -> ""

(* Process the event and transform the state *)
let on_event (state : 'local_state Strategy.state) (event : event) : 'local_state Strategy.state Lwt.t =
  match event with
  | Market_data_event candle ->
    let%lwt option_chain = Option_chain.get () in
    let current_time =  candle.timestamp in
    let candle_lots_limit = state.local_state.candle_lots_limit in
    let lots_sold_for_current_candle = state.local_state.lots_sold_for_current_candle in
    let current_breach =
      get_breach_type ~candle:candle
    in
    (* Cornerstone: we are assuming the option_chain will always have the expiry to be worked upon *)
    let expiry = current_expiry_from_option_chain ~option_chain:option_chain in 
    Printf.printf " current expiry is %s\n" expiry;
     (* Wrap the offset calculation in Lwt.catch to handle possible failure *)
    Lwt.catch
      (fun () ->
        match expiry_to_ptime expiry with
        | Some expiry_ptime ->
          let offset = get_offset_from_day current_time expiry_ptime in
          Lwt.return offset
        | None ->
          Lwt.fail_with ("Invalid expiry date: " ^ expiry))
      (fun exn ->
        (* Log error or handle it, and provide fallback offset *)
        Printf.eprintf "Error parsing expiry date: %s\n" (Printexc.to_string exn);
        Lwt.return 0.0)  (* fallback or handle as per your logic *)
    >>= fun offset ->
    let expired_close_orders = expired_close_orders current_time state.positions option_chain in
    let transition_orders = 
      (if lots_sold_for_current_candle < candle_lots_limit then
        transition_orders
          ~current_breach:current_breach
          ~last_breach:state.local_state.last_breach
          ~candle:candle
          ~option_chain:option_chain
          ~offset:offset
          ~positions:state.positions
        else
          [])
    in
    (* let close_time_orders = *)
    (*   close_time_orders *)
    (*     ~candle:candle *)
    (*     ~positions:state.positions *)
    (*     ~option_chain:option_chain in *)
    (* bug here, what if the expired closed orders/trasnsition orders, generated the same orders as close time orders *)
    
    let all_orders = expired_close_orders @ transition_orders @ state.created_orders (* @ close_time_orders *) in
    let positions = Position.update_positions_with_option_chain option_chain state.positions in
    let new_local_state = { state.local_state with
                            last_breach = current_breach;
                            candle = candle;
                            option_chain = option_chain;
                            lots_sold_for_current_candle = lots_sold_for_current_candle + List.length transition_orders} in
    let new_state = {
      state with created_orders = all_orders;
      local_state = new_local_state;
      positions = positions
    } in

    (* write all positions to csv at the end, hopefully all of them are closed. *)
    if is_time current_time (10 * 60) then (
      write_header_to_csv "pnl.csv";
      List.iter (fun pos -> write_position_to_csv "pnl.csv" pos) state.positions;
      Lwt.return new_state
    ) else
      Lwt.return new_state

let extract_orders (state : 'local_state Strategy.state) : Order.t list * 'local_state Strategy.state =
  let orders_to_extract = state.created_orders in
  let new_state = { state with created_orders = [] } in
  (orders_to_extract, new_state)

(* The final strategy packaged together *)
let create (config : ('local_config, 'local_state) Strategy.config) : ('local_config, 'local_state) Strategy.t =  Strategy.create
  config
  initial_local_state
  extract_orders
