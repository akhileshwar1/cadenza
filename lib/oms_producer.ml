(* lib/oms_producer.ml *)

open Lwt.Infix
open Event_types
open Connector


(* Start an OMS websocket producer that pushes OMS_UPDATE events into [queue].
   - queue: an Event_queue.t (same type used by ring_producer)
   - ws_url: websocket URL string (e.g. "wss://...") for OMS output stream
   - login_msg: string message to send after connect (could be "{}" or login json)
   - heartbeat_msg: heartbeat / ping payload string (sent periodically by connector)
   - stop_ref: bool ref; when set to true you may expect external shutdown (connector usually handles reconnect/close)
*)
let start ~queue ~ws_url ~login_msg ~heartbeat_msg : unit Lwt.t =
  (* raw message handler invoked by connector for each incoming websocket payload *)
  let raw_message_handler (msg : string) : unit Lwt.t =
    (* try parse json; if not parseable ignore but log *)
    match Yojson.Safe.from_string msg with
    | exception Yojson.Json_error _ ->
      Lwt_io.eprintf "[oms_producer] invalid JSON from OMS: %s\n" msg
    | json ->
      let open Yojson.Safe.Util in
      let interested =
        (* Heuristics to detect an order/update/execution report message from OMS/broker *)
        (try
           match json |> member "e" |> to_string with
           | "executionReport" -> true
           | _ -> false
         with _ -> false)
        ||
        (try
           (* some providers nest an "order" object *)
           match json |> member "order" with
           | `Null -> false
           | _ -> true
         with _ -> false)
        ||
        (try
           match json |> member "type" |> to_string with
           | "order_update" | "order" -> true
           | _ -> false
         with _ -> false)
      in
      if not interested then Lwt.return_unit
      else
        let ev = { typ = OMS_UPDATE; payload = json; recv_at = Ptime_clock.now () } in
        (* push to queue; assume Event_queue.push : queue -> Event_types.event -> unit Lwt.t *)
        Event_queue.push queue ev >>= fun () -> Lwt.return_unit
  in

  (* Kick off the connector. It handles the connection lifecycle and will call our raw_message_handler. *)
  connect_to_data_stream ws_url raw_message_handler login_msg heartbeat_msg false
