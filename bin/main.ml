(* open Lwt.Infix *)
open Cadenza.Order
open Lwt
open Cadenza.Connector

let () =
  (* Build config *)
  let config = {
    Cadenza.Strategy.data_layer_uri = "ws://127.0.0.1:8765/";
    oms_layer_uri = "http://yourorderlayer";
    symbol = "NIFTY";
    local_config = ();
  } in

  let strategy = Cadenza.Alternate_strategy.create config in
  let current_strategy = ref strategy in

  let process_json_message (json : Yojson.Safe.t) : unit Lwt.t =
    Lwt_io.printf "Processing JSON: %s\n%!" (Yojson.Safe.to_string json) >>= fun () ->
    (* Use Lwt_io.printf *)
    try
      (* Using the json_to_candle from your strategy module *)
      let candle = Cadenza.Alternate_strategy.json_to_candle json in

      let old_state = (!current_strategy).state in
      (* Using on_event from your strategy module *)
      let new_state = Cadenza.Alternate_strategy.on_event old_state (Cadenza.Alternate_strategy.Market_data_event candle) in
      (* Using update_state from Cadenza.Strategy or your specific module *)
      current_strategy := Cadenza.Strategy.update_state !current_strategy new_state;

      (* Extract orders and print them asynchronously *)
      let orders, new_state_after_extraction = (!current_strategy).Cadenza.Strategy.extract_orders (!current_strategy).Cadenza.Strategy.state in
      (* Update the strategy state with the state returned by extract_orders (which has pending_orders cleared) *)
      current_strategy := Cadenza.Strategy.update_state !current_strategy new_state_after_extraction;

      Lwt_list.iter_s (fun order -> (* Use Lwt_list.iter_s for asynchronous iteration *)
        Lwt_io.printf "New order: %s %d @ %.2f\n%!" (* Use Lwt_io.printf *)
          (match order.side with | Buy -> "BUY" | Sell -> "SELL")
          order.quantity
          order.price
      ) orders >>= fun () ->
      Lwt.return_unit (* Return a resolved promise *)
    with
      (* Add specific error handling for your candle processing if needed *)
      |exn -> Lwt_io.eprintf "Error in process_json_message: %s\n%!" (Printexc.to_string exn) >>= fun () -> Lwt.return_unit (* Use Lwt_io.eprintf and return a promise *)
  in

  (* Wrapper function: Takes string, parses JSON, calls process_json_message, returns unit Lwt.t *)
  let websocket_message_adapter (message_string : string) : unit Lwt.t =
    try
      let json = Yojson.Safe.from_string message_string in
      (* Call your existing asynchronous processing logic *)
      process_json_message json
    with
      | Yojson.Safe.Util.Type_error (msg, j) ->
      Lwt_io.eprintf "JSON Type Error in message: %s\nJSON: %s\n" msg (Yojson.Safe.to_string j)
      >>= fun () -> Lwt.return_unit (* Decide if you want to stop on errors or continue *)
      | Yojson.Json_error msg ->
      Lwt_io.eprintf "JSON Parsing Error: %s\nRaw Message: <<< %s >>>\n" msg message_string
      >>= fun () -> Lwt.return_unit (* Decide if you want to stop on errors or continue *)
      | exn ->
      (* Catch any other unexpected errors during parsing or processing *)
      Lwt_io.eprintf "Unexpected error in websocket_message_adapter: %s\n" (Printexc.to_string exn)
      >>= fun () -> Lwt.return_unit
  in
  Lwt_main.run (connect_to_data_stream config.data_layer_uri websocket_message_adapter)
