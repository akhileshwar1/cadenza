(* open Lwt.Infix *)
open Cadenza.Order
open Lwt
open Cadenza.Connector
open Cadenza.Bb_strategy

(* let mock_order_queue : string Lwt_mvar.t = Lwt_mvar.create_empty () *)
let red str = "\027[31m" ^ str ^ "\027[0m"
let green str = "\027[32m" ^ str ^ "\027[0m"

let strategy_mutex = Lwt_mutex.create ()

(* avoid race condition between on_event and order_update *)
let safe_update f =
  Lwt_mutex.with_lock strategy_mutex (fun () ->
    f ()
  )

(* for db calls *)
let with_db_conn strategy_ref f =
  match !strategy_ref.Cadenza.Strategy.config.db_conn with
  | Some conn -> f conn
  | None -> 
      Logs.err (fun m -> m "DB connection not initialized");
      Lwt.return_unit

let log_position_update (pos : Cadenza.Position.t) =
  let value = pos.value in
  let color = if value <= 0.0 then green else red in
  let message =
    Printf.sprintf "Updated Position: %s | Net_price: %.2f | Qty: %d | Value: %.2f"
      pos.symbol
      pos.net_price
      pos.net_qty
      value
  in
  Lwt_io.printf "%s\n%!" (color message)

(* Function to send a single order to the OMS via HTTP POST *)
let send_order_to_oms (oms_uri : Uri.t) (order : Cadenza.Order.t)
                      (strategy_ref : ('a, 'b) Cadenza.Strategy.t ref)
                      : unit Lwt.t =

  let state = (!strategy_ref).state in
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
        Lwt_io.printf "Order successfully sent to OMS.\n" >>= fun () ->
        let open Yojson.Safe.Util in
        let open Lwt.Syntax in
        let json = Yojson.Safe.from_string body_string in
        let broker_order_id = json |> member "broker_order_id" |> to_string in
        let updated_state = {state with pending_orders = state.pending_orders @ [{order with broker_order_id = broker_order_id}]} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        (* insert the order in db here *)
        with_db_conn 
          strategy_ref
          (fun conn ->
            let order_with_id = { order with broker_order_id } in
            let* res = Cadenza.Order_store.insert conn order_with_id in
            match res with
            | Ok _ -> Lwt.return_unit
            | Error _ ->
              Lwt.return_unit
          )
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
  : (Cadenza.Order.t list * Cadenza.Bb_strategy.candle * Cadenza.Option_chain.t) Lwt.t =
  let empty_candle = {timestamp = ""; open_price = 0.0; high_price = 0.0; low_price = 0.0;
    close_price = 0.0; upper_band = 0.0; lower_band = 0.0; sma = 0.0} in
  try
    let candle = Cadenza.Bb_strategy.json_to_candle json in

    let event = Cadenza.Bb_strategy.Market_data_event candle in (* Assuming Market_data_event is in Cadenza.Strategy *)

    let old_state = (!current_strategy_ref).state in
    let%lwt new_state_after_event = Cadenza.Bb_strategy.on_event old_state event in
    current_strategy_ref := Cadenza.Strategy.update_state !current_strategy_ref new_state_after_event;

    let orders, new_state_after_extraction = Cadenza.Bb_strategy.extract_orders (!current_strategy_ref).Cadenza.Strategy.state in
    current_strategy_ref := Cadenza.Strategy.update_state !current_strategy_ref new_state_after_extraction;
    let option_chain = !current_strategy_ref.state.local_state.option_chain in
    Lwt.return (orders, candle, option_chain)
  with
    (* Add specific error handling for your candle processing if needed *)
    | Yojson.Safe.Util.Type_error (msg, j) ->
          Lwt_io.eprintf "JSON Type Error in process_json_message: %s\nJSON: %s\n" msg (Yojson.Safe.to_string j)
    >>= fun () -> Lwt.return ([], empty_candle , []) 
    | Yojson.Json_error msg ->
    Lwt_io.eprintf "JSON Parsing Error in process_json_message: %s\nRaw Message: <<< %s >>>\n" msg (Yojson.Safe.to_string json) (* Pass the json object for context *)
    >>= fun () -> Lwt.return ([], empty_candle, [])
    | exn ->
    Lwt_io.eprintf "Unexpected error in process_json_message: %s\n" (Printexc.to_string exn)
    >>= fun () -> Lwt.return ([], empty_candle, [])

(* Function to create the actual message handler callback, specific to Bb_strategy *)
(* Side effects here *)
let create_message_handler
  (oms_uri : Uri.t)
  (current_strategy_ref : (unit, Cadenza.Bb_strategy.local_state) Cadenza.Strategy.t ref) (* Specific strategy ref type *)
  : raw_message_callback =
  (* This is the function that will be passed to Websocket_lwt_unix.connect *)
  (fun message_string ->
    Lwt.catch
      (fun () ->
        let (let*) = Lwt.bind in
        (* Parse the raw message string as JSON *)
        let json = Yojson.Safe.from_string message_string in
        (* Process the JSON message using the specific strategy functions and get orders *)
        safe_update (fun () -> process_json_message json current_strategy_ref >>= fun (orders, candle, option_chain) ->

          (* Send extracted orders to OMS *)
          Lwt_io.printf "Extracted %d orders. Sending to OMS...\n" (List.length orders) >>= fun () ->
          Lwt_list.iter_s (fun order -> (* Use Lwt_list.iter_s for asynchronous iteration *)
            (* add the liquidity call here for UAT testing. *)
            (* let counter_order =  *)
            (*   match order.side with *)
            (*   | Buy -> {order with side = Sell} *)
            (*   | Sell -> {order with side = Buy} in *)
            (*      let%lwt _ = send_order_to_oms (Uri.of_string "http://localhost:9001/order/place") counter_order current_strategy_ref in *)
            send_order_to_oms oms_uri order current_strategy_ref (* Call the new function *)
          ) orders >>= fun () ->
          if (candle.timestamp != "") then
            with_db_conn current_strategy_ref (fun conn ->
              Printf.printf "in insert candle\n%!";
              let* res = Cadenza.Candle_store.insert conn candle in
              match res with
              | Ok _ -> 
                Printf.printf "inserted candle\n%!";
                Lwt.return_unit
              | Error err ->
                Logs.err (fun m -> m "DB update failed: %a" Caqti_error.pp err);
                Lwt.return_unit
            )
          else Lwt.return_unit;
          >>= fun () ->
            if (option_chain != [] && candle.timestamp != "") then
              with_db_conn current_strategy_ref (fun conn ->
                Printf.printf "in insert chain\n%!";
                let* res = Cadenza.Option_chain_store.insert conn ~timestamp:candle.timestamp option_chain in
                match res with
                | Ok _ -> 
                  Printf.printf "inserted option chain\n%!";
                  Lwt.return_unit
                | Error err ->
                  Logs.err (fun m -> m "DB update failed: %a" Caqti_error.pp err);
                  Lwt.return_unit
              )
            else Lwt.return_unit
        ))
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
  let (let*) = Lwt.bind in
  try
    match Cadenza.Order.of_yojson json with
    | order ->
      if order.status = Some Cadenza.Order.Completed then
        Lwt_io.printf " In order completed\n" >>= fun () ->
        let pending_order = List.find (fun x -> x.broker_order_id = order.broker_order_id) pending_orders in
        (* add this delta order update to the state we already have with regards fill price and qty *)
        let completed_order = Cadenza.Order.apply_order_update order pending_order in
        let json = json_of_order completed_order in
        Printf.printf " Completed Order is: %s\n%!" (Yojson.Safe.pretty_to_string json);
        let updated_pending_orders = List.filter (fun x -> not (x.broker_order_id = order.broker_order_id)) pending_orders in
        let updated_positions = Cadenza.Position.update_or_insert_position state.positions completed_order "bb" in
        let updated_state = {state with completed_orders = completed_orders @ [completed_order];
          pending_orders = updated_pending_orders;
          positions = updated_positions} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        (* ⬇ Insert DB update here in Lwt context *)
        let* () =
          with_db_conn strategy_ref (fun conn ->
            let* res = Cadenza.Order_store.update conn completed_order in
            match res with
            | Ok _ -> Lwt.return_unit
            | Error err ->
              Logs.err (fun m -> m "DB update failed: %a" Caqti_error.pp err);
              Lwt.return_unit
          )
        in
        (* Log and write only the relevant position *)
        (match (List.find_opt (fun (pos : Cadenza.Position.t) -> pos.symbol = completed_order.tradingsymbol) updated_positions) with
          | Some pos ->
            log_position_update pos |> ignore;
            Cadenza.Bb_strategy.write_position_to_csv "positions.csv" pos
          | None -> ());
        Lwt.return_unit
      else if order.status = Some Cadenza.Order.Rejected || order.status = Some Cadenza.Order.Cancelled then
        let updated_pending_orders = List.filter (fun x -> not (x.broker_order_id = order.broker_order_id)) pending_orders in
        let updated_state = {state with rejected_orders = rejected_orders @ [order];
          pending_orders = updated_pending_orders} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        Lwt.return_unit
      else (* handles partially executed and pending type order updates *)
        Lwt_io.printf " In order update\n" >>= fun () ->
        let updated_pending_orders = List.map (Cadenza.Order.apply_order_update order) pending_orders in
        let pending_order = List.find (fun x -> x.broker_order_id = order.broker_order_id) pending_orders in
        let json = json_of_order pending_order in
        Printf.printf " Updated Pending Order is: %s\n%!" (Yojson.Safe.pretty_to_string json);
        let updated_state = { state with pending_orders = updated_pending_orders} in
        strategy_ref := Cadenza.Strategy.update_state !strategy_ref updated_state;
        let* () =
          with_db_conn strategy_ref (fun conn ->
            let* res = Cadenza.Order_store.update conn pending_order in
            match res with
            | Ok _ -> Lwt.return_unit
            | Error err ->
              Logs.err (fun m -> m "DB update failed: %a" Caqti_error.pp err);
              Lwt.return_unit
          )
        in
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
        safe_update (fun () -> process_order_update json current_strategy_ref)
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
  let open Lwt.Syntax in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);

  (* Initialize DB connection and strategy together *)
  let strategy_promise =
    let* result = Cadenza.Db_init.connect () in
    match result with
    | Error e ->
        Logs.err (fun m -> m "DB connection failed: %a" Caqti_error.pp e);
        Lwt.fail_with "DB connection failed"
    | Ok (module Conn) ->
        let* setup_result = Cadenza.Db_init.setup (module Conn) in
        (match setup_result with
        | Error e ->
            Logs.err (fun m -> m "DB setup failed: %a" Caqti_error.pp e);
            Lwt.fail_with "DB setup failed"
        | Ok () ->
          Printf.printf "Db connected!\n%!";
          (* Build config with db_conn now *)
          let config = {
            Cadenza.Strategy.data_layer_uri = Cadenza.Connector.get_env_or_default "DATA_LAYER_URI" "ws://127.0.0.1:8000/candles/stream";
            oms_layer_uri = Cadenza.Connector.get_env_or_default "OMS_LAYER_URI" "http://localhost:9000/order/place";
            oms_ws_uri = Cadenza.Connector.get_env_or_default "OMS_WS_URI" "ws://localhost:8081/";
            symbol = "NIFTY";
            local_config = ();
            db_conn = Some (module Conn);  (* Inject the DB connection *)
          } in
          let strategy = Cadenza.Bb_strategy.create config in
          Lwt.return strategy)
  in

  (* Compose Lwt main after strategy is initialized *)
  let main =
    let* strategy = strategy_promise in
    let current_strategy = ref strategy in

    let oms_uri = Uri.of_string strategy.config.oms_layer_uri in

    let message_handler =
      create_message_handler
        oms_uri
        current_strategy
    in

    let order_update_handler =
      create_order_update_handler
        current_strategy
    in

    let login_msg = Yojson.Safe.to_string (`Assoc []) in
    let heartbeat_msg = Yojson.Safe.to_string (`Assoc []) in

    let market_data_promise =
      connect_to_data_stream
        strategy.config.data_layer_uri
        message_handler
        login_msg
        heartbeat_msg
        false
    in

    let oms_update_promise =
      connect_to_data_stream
        strategy.config.oms_ws_uri
        order_update_handler
        login_msg
        heartbeat_msg
        false
    in

    Cadenza.Bb_strategy.write_header_to_csv "positions.csv";

    Lwt.join [market_data_promise; oms_update_promise]
  in

  Lwt_main.run main
