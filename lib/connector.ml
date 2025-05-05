open Lwt.Infix
open Websocket_lwt_unix
(* open Uri (* Still need this for Uri parsing functions *) *)
(* open Conduit_lwt_unix (* Need this for Conduit types and functions for the connect method *) *)
(* open Resolver_lwt (* Need this for Resolver_lwt.system and its 'resolve' method *) *)
(* open Resolver_lwt_unix (* Need this for Resolver_lwt_unix.system_service *) *)
(* open Result (* For handling results *) *)
(* open Ipaddr (* Need this for IP address types used in Conduit.endp *) *)
(* open Lwt_unix (* Need this for getaddrinfo *) *)
(* open Ipaddr_unix (* Need this to convert Unix.inet_addr to Ipaddr.t *) *)
(* open Cohttp *)
(* open Cohttp_lwt_unix *)

(* Instantiate the Resolver functor with Cohttp_lwt_unix.IO *)
module R = Resolver.Make(Cohttp_lwt_unix.IO)

(* Define the type for the callback expected by connect_to_data_stream *)
type raw_message_callback = string -> unit Lwt.t

(* Custom service lookup function to handle ws/wss explicitly *)
let custom_service_handler name =
  match name with
  | "ws" ->
    (* WebSocket over TCP - default port 80, no TLS *)
    let svc = { Resolver.name = "ws"; port = 80; tls = false } in
    Lwt.return (Some svc)
  | "wss" ->
    (* WebSocket over TLS - default port 443, TLS required *)
    let svc = { Resolver.name = "wss"; port = 443; tls = true } in
    Lwt.return (Some svc)
  | _ ->
    (* For other schemes, fall back to the system's service lookup *)
    (* Using Resolver_lwt_unix.system_service which should be available *)
    Resolver_lwt_unix.system_service name

(* Helper function to get host from URI, defaulting to localhost *)
let get_host uri =
  match Uri.host uri with
  | None -> "localhost"
  | Some host -> (
    match Ipaddr.of_string host with
    | Ok ip -> Ipaddr.to_string ip
    | Error _ -> host)

(* Helper function to get port from URI, defaulting to service port *)
let get_port service uri =
  match Uri.port uri with None -> service.Resolver.port | Some port -> port

(* Rewrite function based on system resolution (copied from resolver_lwt_unix.ml) *)
let system_resolver service uri =
  let open Lwt_unix in
  let host = get_host uri in
  let port = get_port service uri in
  Lwt_io.printf "system_resolver: Resolving host '%s' port %d...\n" host port >>= fun () ->
  Lwt.catch
    (fun () ->
      getaddrinfo host (string_of_int port) [ AI_SOCKTYPE SOCK_STREAM ]
      >>= fun addrinfos ->
      (* In case both IPv4 and IPv6 addresses exist, favor IPv4: *)
      let v4, rest = List.partition (fun i -> i.ai_family = PF_INET) addrinfos in
      match List.rev_append v4 rest with
      | [] ->
        Lwt_io.eprintf "system_resolver: Host resolution failed for '%s'.\n" host >>= fun () ->
        Lwt.return (`Unknown ("name resolution failed for " ^ host))
      | { ai_addr = ADDR_INET (addr, resolved_port); _ } :: _ ->
        Lwt_io.printf "system_resolver: Resolved to INET address.\n" >>= fun () ->
        (* Check if TLS is required based on the service *)
        Lwt.return (`TCP (Ipaddr_unix.of_inet_addr addr, resolved_port))
      | { ai_addr = ADDR_UNIX file; _ } :: _ ->
        Lwt_io.printf "system_resolver: Resolved to Unix domain socket.\n" >>= fun () ->
        Lwt.return (`Unix_domain_socket file)
    )
    (fun exn ->
      let error_msg = Printexc.to_string exn in
      Lwt_io.eprintf "system_resolver: Exception during resolution for '%s': %s\n" host error_msg >>= fun () ->
      Lwt.return (`Unknown ("exception during resolution for " ^ host ^ ": " ^ error_msg))
    )

let rec heartbeat_loop conn interval_seconds heartbeat_msg =
  let open Lwt.Infix in
  let frame = Websocket.Frame.create ~opcode:Text ~content:heartbeat_msg () in
  Websocket_lwt_unix.write conn frame >>= fun () ->
  Lwt_unix.sleep (float_of_int interval_seconds) >>= fun () ->
  heartbeat_loop conn interval_seconds heartbeat_msg

(* The main function to connect and handle messages *)
let connect_to_data_stream (uri_string : string) (on_raw_message : raw_message_callback) (login_msg : string)
  (heartbeat_msg : string): unit Lwt.t =
  let uri = Uri.of_string uri_string in
  (* Extract scheme for validation *)
  let scheme = Uri.scheme uri in
  (* Host, port, path are handled by the resolver *)

  (* Optional: Basic validation that the scheme is ws or wss - Keep this check early *)
  let () = match scheme with
    | Some "ws" | Some "wss" -> ()
    | _ -> failwith (Printf.sprintf "Unsupported URI scheme: %s. Must be ws or wss." (match scheme with Some s -> s | None -> "none"))
  in

  (* Define the recursive function to continuously read messages *)
  let rec read_loop (conn : conn) : unit Lwt.t =
    Lwt.catch
      (fun () ->
        Lwt_io.printf " in read loop " >>= fun () ->
        (* Read one frame from the websocket - Disambiguated call *)
        Websocket_lwt_unix.read conn >>= fun frame ->
        (* Optional: Log received frame details for debugging *)
        Lwt_io.printf "<- %s\n" (Websocket.Frame.show frame) >>= fun () ->

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
          Websocket_lwt_unix.write conn pong_frame >>= fun () ->
          Lwt_io.printl "...PONG sent." >>= fun () ->
          read_loop conn (* Continue reading *)

        | Websocket.Frame.Opcode.Close ->
          (* Server initiated close *)
          (* Manually extract code and reason from content *)
          let code, reason =
            if String.length frame.content >= 2 then (
              (* Extract 16-bit code from the first two bytes (Network Byte Order) *)
              let hi = Char.code frame.content.[0] in
              let lo = Char.code frame.content.[1] in
              let code = (hi lsl 8) lor lo in
              (* Extract reason string from remaining bytes *)
              let reason = String.sub frame.content 2 (String.length frame.content - 2) in
              (Some code, reason)
            ) else (
              (* No code or reason provided in the payload *)
              (None, "")
            )

          in
          Lwt_io.printf "Connection closed by server (code: %s, reason: '%s')\n"
            (match code with Some c -> string_of_int c | None -> "none") reason >>= fun () -> (* Print extracted code and reason *)
          (* Respond with a close frame and exit the loop. *)
          (* Determine the code to send back *)
          let response_code = match code with
            | Some c -> c (* Use the code received if present *)
            | None -> 1000 (* Default to 1000 (Normal Closure) if no code received *)
          in
          (* The Lwt.catch around write handles if the connection is already broken *)
          Lwt.catch
            (fun () ->
              (* Create a response close frame using the determined code *)
              (* Websocket.Frame.close only takes an int according to websocket.mli *)
              let response_frame = Websocket.Frame.close response_code in
              write conn response_frame
            )
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
          (fun () -> Websocket_lwt_unix.write conn (Websocket.Frame.close 1011)) (* Internal error *)
          (fun _ -> Lwt.return_unit) (* Ignore errors during close attempt *)
        >>= fun () -> Lwt.return_unit (* Exit the loop after error *)
      )
  in

  (* Initialize the random number generator for TLS *)
  Mirage_crypto_rng_unix.use_default ();

  (* Establish the connection - Using Resolver_lwt.init with custom service and system rewrite handlers *)
  Lwt_io.printf "Attempting to connect to %s...\n" (Uri.to_string uri) >>= fun () ->
  Lwt.catch
    (fun () ->
      Lwt_io.printl "Step 1: Starting URI resolution with custom service and system rewrite handlers..." >>= fun () ->
      (* 1. Initialize the Resolver_lwt module SYNCHRONOUSLY, providing the custom service handler and system rewrite rule *)
      let resolver_inst = Resolver_lwt.init
        ~service:custom_service_handler
        ~rewrites:[ ("", system_resolver) ] (* Provide system_resolver as the default rewrite rule *)
        ()
      in

      (* 2. Resolve the Uri to a Conduit.endp using the resolver instance (asynchronous) *)
      (* This function returns Conduit.endp Lwt.t *)
      Resolver_lwt.resolve_uri ~uri resolver_inst >>= fun conduit_endp ->
      Lwt_io.printf "Step 1: URI resolution complete. Endpoint: %s\n" (Sexplib0.Sexp.to_string_hum (Conduit.sexp_of_endp conduit_endp)) >>= fun () ->

      Lwt_io.printl "Step 2: Initializing Conduit context..." >>= fun () ->
      (* 2. Initialize Conduit context (asynchronous) *)
      Conduit_lwt_unix.init () >>= fun ctx ->
      Lwt_io.printl "Step 2: Conduit context initialized." >>= fun () ->

      Lwt_io.printl "Step 3: Converting endpoint to client..." >>= fun () ->
      (* 3. Convert the Conduit.endp to a Conduit_lwt_unix.client using ctx (asynchronous) *)
      Conduit_lwt_unix.endp_to_client ~ctx conduit_endp >>= fun client ->
      Lwt_io.printl "Step 3: Endpoint converted to client." >>= fun () ->

      Lwt_io.printl "Step 4: Connecting via Websocket_lwt_unix..." >>= fun () ->
      (* 4. Connect using Websocket_lwt_unix.connect with the client and original Uri.t (asynchronous) *)
      Websocket_lwt_unix.connect client uri >>= fun conn ->
      Lwt_io.printl "Step 4: Websocket connection initiated." >>= fun () ->


      Lwt_io.printf "WebSocket connection established.\n" >>= fun () ->
      Lwt.async (fun () -> read_loop conn); (* Start reading ASAP *)
      let login_frame = Websocket.Frame.create ~opcode:Text ~content:login_msg () in
      Websocket_lwt_unix.write conn login_frame >>= fun () ->
      Lwt_io.printf " sent login frame %s \n " login_msg >>= fun () ->
      Lwt.async (fun () -> heartbeat_loop conn 10 heartbeat_msg); (* 10 second interval *)
      Lwt.return_unit
    )  (* This is the closing parenthesis of the try block function *)
    (fun exn ->
      (* Handle connection errors, including potential exceptions from resolution *)
      let error_msg = Printexc.to_string exn in
      Lwt_io.eprintf "Failed during connection setup: %s\n" error_msg >>= fun () ->
      Lwt.return_unit (* Return a resolved promise indicating failure *)
    ) (* This is the closing parenthesis of the catch block function *)
