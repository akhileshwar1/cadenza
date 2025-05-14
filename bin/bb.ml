(* open Lwt.Infix *)
open Cadenza.Order
open Lwt
open Cadenza.Connector

(* let mock_order_queue : string Lwt_mvar.t = Lwt_mvar.create_empty () *)
let red str = "\027[31m" ^ str ^ "\027[0m"
let green str = "\027[32m" ^ str ^ "\027[0m"

let log_position_update (pos : Cadenza.Position.t) =
  let value = pos.value in
  let color = if value <= 0.0 then green else red in
  let message = Printf.sprintf "Updated Position: %s | Value: %.2f" pos.symbol value in
  Lwt_io.printf "%s\n%!" (color message)

let write_position_to_csv (pos : Cadenza.Position.t) (file : string) =
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 file in
  let tm = Unix.localtime pos.opened_at_epoch in
  let timestamp =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
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

(* Function to send a single order to the OMS via HTTP POST *)
let send_order_to_oms (oms_uri : Uri.t) (order : Cadenza.Order.t) : unit Lwt.t =
  Lwt_io.printf "Attempting to send order: %s %s %d @ %.2f to OMS...\n"
    order.tradingsymbol
    (match order.side with | Buy -> "BUY" | Sell -> "SELL")
    order.quantity
    order.price >>= fun () ->

  (* Convert order to JSON *)
  let order_json = json_of_order order in
  (* let updated_json = *)
  (*   match order_json with *)
  (*   | `Assoc fields -> *)
  (*     `Assoc (("order_status", `String "COMPLETED") :: fields) *)
  (*   | _ -> *)
  (*     order_json *)
  (* in *)
  let order_body = Yojson.Safe.to_string order_json in

  (* Construct the HTTP request *)
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string order_body in
  let meth = `POST in

  Lwt_io.printf "Sending POST request to %s with body: %s\n" (Uri.to_string oms_uri) order_body >>= fun () ->

  Lwt_io.printf " putting in the order update\n" >>= fun() ->
  (* Lwt_mvar.put mock_order_queue order_body >>= fun () -> *)

  (* Send the request and handle the response *)
  Lwt.catch
    (fun () ->
      Cohttp_lwt_unix.Client.call meth oms_uri ~headers ~body >>= fun (resp, body) ->
      let status = Cohttp.Response.status resp in
      let status_int = Cohttp.Code.code_of_status status in
      let status_string = Cohttp.Code.string_of_status status in

      Lwt_io.printf "OMS Response Status: %d %s\n" status_int status_string >>= fun () ->

      Cohttp_lwt.Body.to_string body >>= fun body_string ->
      Lwt_io.printf "OMS Response Body: %s\n" body_string >>= fun () ->

      if Cohttp.Code.is_success status_int then (
        Lwt_io.printf "Order successfully sent to OMS.\n"
      ) else (
        Lwt_io.eprintf "OMS returned an error status: %d %s\n" status_int status_string
      )
          )
    (fun exn ->
      let error_msg = Printexc.to_string exn in
      Lwt_io.eprintf "Error sending order to OMS: %s\n" error_msg
    )


(* Your message processing logic, now specific to Bb_strategy *)
let process_json_message
  (json : Yojson.Safe.t)
  (current_strategy_ref : (unit, Cadenza.Bb_strategy.local_state) Cadenza.Strategy.t ref) (* Specific strategy ref type *)
  : Cadenza.Order.t list Lwt.t =
  try
    let candle = Cadenza.Bb_strategy.json_to_candle json in
    let event = Cadenza.Bb_strategy.Market_data_event candle in (* Assuming Market_data_event is in Cadenza.Strategy *)

    let old_state = (!current_strategy_ref).state in
    let%lwt new_state_after_event = Cadenza.Bb_strategy.on_event old_state event in
    current_strategy_ref := Cadenza.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = Cadenza.Bb_strategy.extract_orders (!current_strategy_ref).Cadenza.Strategy.state in
    current_strategy_ref := Cadenza.Strategy.update_state !current_strategy_ref new_state_after_extraction;

    Lwt.return orders
  with
    (* Add specific error handling for your candle processing if needed *)
    | Yojson.Safe.Util.Type_error (msg, j) ->
    Lwt_io.eprintf "JSON Type Error in process_json_message: %s\nJSON: %s\n" msg (Yojson.Safe.to_string j)
    >>= fun () -> Lwt.return []
    | Yojson.Json_error msg ->
    Lwt_io.eprintf "JSON Parsing Error in process_json_message: %s\nRaw Message: <<< %s >>>\n" msg (Yojson.Safe.to_string json) (* Pass the json object for context *)
    >>= fun () -> Lwt.return []
    | exn ->
    Lwt_io.eprintf "Unexpected error in process_json_message: %s\n" (Printexc.to_string exn)
    >>= fun () -> Lwt.return []

(* Function to create the actual message handler callback, specific to Bb_strategy *)
let create_message_handler
  (oms_uri : Uri.t)
  (current_strategy_ref : (unit, Cadenza.Bb_strategy.local_state) Cadenza.Strategy.t ref) (* Specific strategy ref type *)
  : raw_message_callback =
  (* This is the function that will be passed to Websocket_lwt_unix.connect *)
  (fun message_string ->
    Lwt.catch
      (fun () ->
        (* Parse the raw message string as JSON *)
        let json = Yojson.Safe.from_string message_string in

        (* Process the JSON message using the specific strategy functions and get orders *)
        process_json_message json current_strategy_ref >>= fun orders ->

        (* Send extracted orders to OMS *)
        Lwt_io.printf "Extracted %d orders. Sending to OMS...\n" (List.length orders) >>= fun () ->
        Lwt_list.iter_s (fun order -> (* Use Lwt_list.iter_s for asynchronous iteration *)
          send_order_to_oms oms_uri order (* Call the new function *)
        ) orders
      )
      (fun exn ->
        let error_msg = Printexc.to_string exn in
        Lwt_io.eprintf "Error in message handler: %s\n" error_msg >>= fun () ->
        Lwt.return_unit (* Continue processing other messages *)
      )
  )

let process_order_update
  (json : Yojson.Safe.t)
  (strategy_ref : ('a, 'b) Cadenza.Strategy.t ref)
  : unit Lwt.t =

  let state = (!strategy_ref).state in
  let pending_orders = state.pending_orders in
  let completed_orders = state.completed_orders in
  let rejected_orders = state.rejected_orders in
  try
    match Cadenza.Order.of_yojson json with
    | order ->
      if order.status == Some Cadenza.Order.Completed then
        let updated_pending_orders = List.filter (fun x -> (x.order_id = order.order_id)) pending_orders in
        let updated_positions = Cadenza.Position.update_or_insert_position state.positions order "bb" in
        let updated_state = {state with completed_orders = completed_orders @ [order];
          pending_orders = updated_pending_orders;
          positions = updated_positions} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        (* Log and write only the relevant position *)
        (match (List.find_opt (fun (pos : Cadenza.Position.t) -> pos.symbol = order.tradingsymbol) updated_positions) with
          | Some pos ->
            log_position_update pos |> ignore;
            write_position_to_csv pos "trades.csv"
          | None -> ());
        Lwt.return_unit
      else if order.status == Some Cadenza.Order.Rejected || order.status == Some Cadenza.Order.Cancelled then
        let updated_pending_orders = List.filter (fun x -> (x.order_id = order.order_id)) pending_orders in
        let updated_state = {state with rejected_orders = rejected_orders @ [order];
          pending_orders = updated_pending_orders} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        Lwt.return_unit
      else
        Lwt_io.printf " in order update\n" >>= fun () ->
        let updated_pending_orders = List.map (fun x -> if x.order_id == order.order_id then
          {x with filled_quantity = order.filled_quantity;
            status = order.status}
          else
            x)
          pending_orders in
        let updated_state = { state with pending_orders = updated_pending_orders} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        Lwt.return_unit
  with
    | exn ->
    Lwt_io.eprintf "Exception in process_order_update: %s\n" (Printexc.to_string exn)

let create_order_update_handler
  (current_strategy_ref : (unit, Cadenza.Bb_strategy.local_state) Cadenza.Strategy.t ref) (* Specific strategy ref type *)
  : raw_message_callback =
  (* This is the function that will be passed to Websocket_lwt_unix.connect *)
  (fun message_string ->
    Lwt.catch
      (fun () ->
        (* Parse the raw message string as JSON *)
        let json = Yojson.Safe.from_string message_string in

        (* Process the JSON message using the specific strategy functions and get orders *)
        process_order_update json current_strategy_ref 
      )
      (fun exn ->
        let error_msg = Printexc.to_string exn in
        Lwt_io.eprintf "Error in message handler: %s\n" error_msg >>= fun () ->
        Lwt.return_unit (* Continue processing other messages *)
      )
  )

(* let start_mock_order_feeder (handler : string -> unit Lwt.t) = *)
(*   let rec loop () = *)
(*     Lwt_io.printf " started mock order feeder\n" >>= fun () -> *)
(*     Lwt_mvar.take mock_order_queue >>= fun order_str -> *)
(*     handler order_str >>= loop *)
(*   in *)
(*   Lwt.async loop; *)
(*   Lwt.return_unit *)

let () =
  (* Build config *)
  let config = {
    Cadenza.Strategy.data_layer_uri = "ws://127.0.0.1:8000/candles/stream";
    oms_layer_uri = "http://localhost:9000/order/place";
    oms_ws_uri = "ws://localhost:8081/";
    symbol = "NIFTY";
    local_config = ();
  } in


  let oms_uri = Uri.of_string config.oms_layer_uri in
  let strategy = Cadenza.Bb_strategy.create config in
  let current_strategy = ref strategy in

  (* Create the message handler using the OMS URI and the specific strategy ref *)
  let message_handler =
    create_message_handler
      oms_uri
      current_strategy (* Pass the specific strategy ref *)
  in

  let order_update_handler = 
    create_order_update_handler
      current_strategy
  in

  let login_msg = Yojson.Safe.to_string (`Assoc []) in
  let heartbeat_msg = Yojson.Safe.to_string (`Assoc []) in
  let market_data_promise =
    connect_to_data_stream
      config.data_layer_uri
      message_handler 
      login_msg
      heartbeat_msg
      false
  in

  let oms_update_promise =
    connect_to_data_stream
      config.oms_ws_uri
      order_update_handler
      login_msg
      heartbeat_msg
      false
  in

  (* let mock_order_feeder_promise = *)
  (*   start_mock_order_feeder order_update_handler *)
  (* in *)

  let _ = write_header_to_csv "trades.csv" in

  Lwt_main.run (Lwt.join [market_data_promise; oms_update_promise(* ; mock_order_feeder_promise *)])
