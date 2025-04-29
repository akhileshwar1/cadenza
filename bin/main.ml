(* open Lwt.Infix *)
open Cadenza.Order
open Lwt
open Cadenza.Connector

(* Function to send a single order to the OMS via HTTP POST *)
let send_order_to_oms (oms_uri : Uri.t) (order : Cadenza.Order.t) : unit Lwt.t =
  Lwt_io.printf "Attempting to send order: %s %d @ %.2f to OMS...\n"
    (match order.side with | Buy -> "BUY" | Sell -> "SELL")
    order.quantity
    order.price >>= fun () ->

  (* Convert order to JSON *)
  let order_json = json_of_order order in
  let order_body = Yojson.Safe.to_string order_json in

  (* Construct the HTTP request *)
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string order_body in
  let meth = `POST in

  Lwt_io.printf "Sending POST request to %s with body: %s\n" (Uri.to_string oms_uri) order_body >>= fun () ->

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


(* Your message processing logic, now specific to Alternate_strategy *)
let process_json_message
  (json : Yojson.Safe.t)
  (current_strategy_ref : (unit, Cadenza.Alternate_strategy.local_state) Cadenza.Strategy.t ref) (* Specific strategy ref type *)
  : Cadenza.Order.t list Lwt.t =
  try
    let candle = Cadenza.Alternate_strategy.json_to_candle json in
    let event = Cadenza.Alternate_strategy.Market_data_event candle in (* Assuming Market_data_event is in Cadenza.Strategy *)

    let old_state = (!current_strategy_ref).state in
    let new_state_after_event = Cadenza.Alternate_strategy.on_event old_state event in
    current_strategy_ref := Cadenza.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = Cadenza.Alternate_strategy.extract_orders (!current_strategy_ref).Cadenza.Strategy.state in
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

(* Function to create the actual message handler callback, specific to Alternate_strategy *)
let create_message_handler
  (oms_uri : Uri.t)
  (current_strategy_ref : (unit, Cadenza.Alternate_strategy.local_state) Cadenza.Strategy.t ref) (* Specific strategy ref type *)
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

let () =
  (* Build config *)
  let config = {
    Cadenza.Strategy.data_layer_uri = "ws://127.0.0.1:8765/";
    oms_layer_uri = "http://localhost:9000/order/place";
    symbol = "NIFTY";
    local_config = ();
  } in


  let oms_uri = Uri.of_string config.oms_layer_uri in
  let strategy = Cadenza.Alternate_strategy.create config in
  let current_strategy = ref strategy in

  (* Create the message handler using the OMS URI and the specific strategy ref *)
  let message_handler =
    create_message_handler
      oms_uri
      current_strategy (* Pass the specific strategy ref *)
  in

  Lwt_main.run (connect_to_data_stream config.data_layer_uri message_handler)
