(* lib/ring_producer.ml *)
open Lwt.Infix
open Event_types

(* Use the higher-level wrapper from ring_stub.ml *)
module Ring_stub = Ring_stub

let now_us () =
  Int64.of_float (Unix.gettimeofday () *. 1_000_000.)

let int_to_msg_type = function
  | 1 -> DEPTH_UPDATE
  | 2 -> SNAPSHOT
  | n -> CUSTOM n

(** Start a ring reader loop that pushes into [queue]. *)
let start ~queue ~ring_path ~stop_ref =
  let handle = Ring_stub.open_ring ~path:ring_path in

  let cleanup () =
    try Ring_stub.close_ring handle with _ -> ()
  in

  let rec loop () =
    if !stop_ref then (
      cleanup ();
      Lwt.return_unit
    ) else
      Ring_stub.read_next_lwt handle >>= fun opt ->
      match opt with
      | None ->
          (* no message; small sleep to avoid busy loop *)
          Lwt_unix.sleep 0.005 >>= loop
      | Some (msg_type_int, payload) ->
          let recv_ts_us = now_us () in
          let msg_type = int_to_msg_type msg_type_int in
          (try
             let json = Yojson.Safe.from_string payload in
             let ev = { typ = msg_type; payload = json; recv_ts_us } in
             Event_queue.push queue ev >>= fun () -> Lwt.return_unit
           with Yojson.Json_error _ ->
             Lwt_io.eprintf "[ring_producer] invalid JSON payload\n")
          >>= fun () -> loop ()
  in
  loop ()
