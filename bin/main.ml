open Lwt.Infix
open Cadenza.Order
open Lwt
open Websocket_lwt_unix
open Conduit_lwt_unix

open Uri (* Still need this for Uri parsing functions *)
open Conduit_lwt_unix (* Need this for Conduit types and functions for the connect method *)
open Result (* For handling results *)
open Ipaddr (* Need this for IP address types used in Conduit.endp *)
open Lwt_unix (* Need this for getaddrinfo *)
open Ipaddr_unix (* Need this to convert Unix.inet_addr to Ipaddr.t *)


(* Define the type for the callback expected by connect_to_data_stream *)
type raw_message_callback = string -> unit Lwt.t

(* Helper function to determine and construct the Conduit endpoint based on URI scheme *)
let resolve_endpoint_lwt ~scheme ~host_string ~port =
  match scheme with
  | Some "ws" ->
      (* For ws, resolve hostname to an IP using Lwt_unix.getaddrinfo *)
      Lwt_unix.getaddrinfo host_string (string_of_int port) [Lwt_unix.AI_SOCKTYPE Lwt_unix.SOCK_STREAM] >>= function
      | [] ->
          Lwt_io.eprintf "Failed to resolve host '%s' for ws: No addresses found\n" host_string >>= fun () ->
          Lwt.fail_with ("Hostname resolution failed: No addresses found for " ^ host_string)
      | addr_info :: _ ->
          (* Take the first address info and extract the IP address *)
          match addr_info.Lwt_unix.ai_addr with
          | Lwt_unix.ADDR_INET (ipaddr, _) ->
              (* Construct the TCP endpoint with the resolved IP and port *)
              let tcp_endp : Conduit.endp = `TCP (Ipaddr_unix.of_inet_addr ipaddr, port) in
              Lwt.return tcp_endp
          | _ ->
              Lwt_io.eprintf "Failed to resolve host '%s' for ws: Resolved address is not an INET address\n" host_string >>= fun () ->
              Lwt.fail_with ("Hostname resolution failed: Non-INET or unsupported address resolved for " ^ host_string)

  | Some "wss" ->
      (* For wss, manually construct the TLS endpoint with hostname and port *)
      let tls_endp : Conduit.endp = `TLS (host_string, port) in
      Lwt.return tls_endp

  | _ ->
      (* This case should be caught by the initial check *)
      Lwt.fail_with (Printf.sprintf "Unsupported scheme during endpoint resolution: %s" (match scheme with Some s -> s | None -> "none"))

(* The main function to connect and handle messages *)
let connect_to_data_stream (uri_string : string) (on_raw_message : raw_message_callback) : unit Lwt.t =
  let uri = Uri.of_string uri_string in
  (* Extract host, scheme, and path *)
  let host_string = Uri.host_with_default uri in
  let scheme = Uri.scheme uri in
  let path = Uri.path_and_query uri in

  (* Ensure scheme is websocket - Keep this check early *)
  let () = match scheme with
    | Some "ws" | Some "wss" -> ()
    | _ -> failwith (Printf.sprintf "Unsupported URI scheme: %s. Must be ws or wss." (match scheme with Some s -> s | None -> "none"))
  in

  (* Define the recursive function to continuously read messages *)
  let rec read_loop (conn : conn) : unit Lwt.t =
    Lwt.catch
      (fun () ->
        (* Read one frame from the websocket - Disambiguated call *)
        Websocket_lwt_unix.read conn >>= fun frame ->
        (* Optional: Log received frame details for debugging *)
        (* Lwt_io.printf "<- %s\n" (Websocket.Frame.show frame) >>= fun () -> *)

        match frame.opcode with
        | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary ->
          (* Received a text or binary message *)
          (* Lwt_io.printf "Raw message (length %d)\n" (String.length frame.content) >>= fun () -> *)
          (* Call the user-provided callback with the RAW message content *)
          on_raw_message frame.content >>= fun () ->
          read_loop conn (* Continue reading *)

        | Websocket.Frame.Opcode.Ping ->
          (* Server sent a Ping, respond with Pong *)
           Lwt_io.printl "Received PING, sending PONG" >>= fun () ->
          let pong_frame = Websocket.Frame.create ~opcode:Pong () in
          write conn pong_frame >>= fun () ->
          read_loop conn (* Continue reading *)

        | Websocket.Frame.Opcode.Close ->
          (* Server initiated close *)
          Lwt_io.printf "Connection closed by server (code: %d, reason: '%s')\n"
            (match frame.code with Some c -> c | None -> -1) frame.content >>= fun () -> (* Handle optional code *)
          (* Respond with a close frame and exit the loop. *)
          (* The Lwt.catch around write handles if the connection is already broken *)
          Lwt.catch
           (fun () -> write conn (Websocket.Frame.close frame.code)) (* Use original code for response *)
           (fun _ -> Lwt.return_unit) (* Ignore errors during close attempt *)
          >>= fun () -> Lwt.return_unit (* Exit the loop *)


        | Websocket.Frame.Opcode.Pong ->
          (* Server sent a Pong *)
          Lwt_io.printl "Received PONG" >>= fun () ->
          read_loop conn (* Ignore and continue reading *)

        | _ ->
          (* Handle other unexpected frame types *)
          Lwt_io.eprintf "Warning: Received unhandled frame type: %s\n"
            (Websocket.Frame.Opcode.to_string frame.opcode) >>= fun () ->
          read_loop conn (* Continue reading *)
       )
      (fun exn ->
         (* Handle exceptions during read, including connection closure *)
         let error_msg = Printexc.to_string exn in
         Lwt_io.eprintf "Error during WebSocket read: %s\n" error_msg >>= fun () ->
         (* Attempt to send a close frame (Internal error) if possible.
          The Lwt.catch handles if the connection is already closed. *)
         Lwt.catch
           (fun () -> write conn (Websocket.Frame.close 1011)) (* Internal error *)
           (fun _ -> Lwt.return_unit) (* Ignore errors during close attempt *)
         >>= fun () -> Lwt.return_unit (* Exit the loop after error *)
      )
  in

  (* Establish the connection - Manual approach using low-level resolution *)
  Lwt_io.printf "Attempting to connect to %s...\n" (Uri.to_string uri) >>= fun () ->
  Lwt.catch
    (fun () ->
        (* Get the port, handling defaults based on scheme if missing. *)
        (match Uri.port uri with
         | Some p -> Lwt.return p (* Return a promise resolved to int *)
         | None -> (
            match scheme with
            | Some "ws" -> Lwt.return 80 (* Return a promise resolved to int *)
            | Some "wss" -> Lwt.return 443 (* Return a promise resolved to int *)
            | _ ->
              (* This branch performs Lwt I/O and returns a promise *)
              Lwt_io.eprintf "Warning: Port not specified and cannot infer default for scheme: %s. Using default 80.\n" (match scheme with Some s -> s | None -> "none") >>= fun () ->
              Lwt.return 80 (* Return a promise resolved to int *)
         )
        )
        >>= fun port -> (* Bind here to get the integer port value *)

        (* 1. Initialize Conduit context *)
        Conduit_lwt_unix.init () >>= fun ctx ->

        (* 2. Determine and construct the Conduit.endp using the helper function *)
        resolve_endpoint_lwt ~scheme ~host_string ~port >>= fun conduit_endp ->

        (* 3. Convert the Conduit.endp to a Conduit_lwt_unix.client using ctx *)
        Conduit_lwt_unix.endp_to_client ~ctx conduit_endp >>= fun client ->

        (* 4. Connect using Websocket_lwt_unix.connect with the client and original Uri.t *)
        Websocket_lwt_unix.connect client uri >>= fun conn ->

          Lwt_io.printf "WebSocket connection established.\n" >>= fun () ->
          (* Start the read loop after successful connection *)
          read_loop conn
    )
    (fun exn ->
      (* Handle connection errors *)
      let error_msg = Printexc.to_string exn in
      Lwt_io.eprintf "Failed to connect to WebSocket: %s\n" error_msg >>= fun () ->
      Lwt.return_unit (* Return a resolved promise indicating failure *)
    )


let () =
  (* Build config *)
  let config = {
    Cadenza.Strategy.data_layer_uri = "wss://yourdatastream";
    oms_layer_uri = "http://yourorderlayer";
    symbol = "NIFTY";
    local_config = ();
  } in

  let strategy = Cadenza.Alternate_strategy.create config in
  let current_strategy = ref strategy in

  (* Your original message handler logic (synchronous, takes Yojson.Safe.t) *)
  let process_json_message (json : Yojson.Safe.t) : unit =
     (* Printf.printf "Processing JSON: %s\n%!" (Yojson.Safe.to_string json); *)
    try
      let candle = Cadenza.Alternate_strategy.json_to_candle json in

      let old_state = (!current_strategy).state  in
      let new_state = Cadenza.Alternate_strategy.on_event old_state (Market_data_event candle) in
      current_strategy := Cadenza.Strategy.update_state !current_strategy new_state;

      (* Extract orders and print them *)
      let orders = (!current_strategy).Cadenza.Strategy.extract_orders (!current_strategy).Cadenza.Strategy.state in
      List.iter (fun order ->
        Printf.printf "New order: %s %d @ %.2f\n%!" (* %! forces printf to flush *)
          (match order.side with | Buy -> "BUY" | Sell -> "SELL")
          order.quantity
          order.price
      ) orders
    with
    (* Add specific error handling for your candle processing if needed *)
    | exn -> Printf.eprintf "Error in process_json_message: %s\n%!" (Printexc.to_string exn)
  in

  (* Wrapper function: Takes string, parses JSON, calls process_json_message, returns unit Lwt.t *)
  let websocket_message_adapter (message_string : string) : unit Lwt.t =
    try
      let json = Yojson.Safe.from_string message_string in
      (* Call your existing synchronous processing logic *)
      process_json_message json;
      (* Return a resolved Lwt promise as required *)
      Lwt.return_unit
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
  Lwt_main.run (connect_to_data_stream config.data_layer_uri on_message)
