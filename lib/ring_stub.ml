(*
  ring_stub.ml
  Thin OCaml wrapper around your C stub. Provides a non-blocking Lwt read helper
  by using Lwt_preemptive.detach to call the blocking C function.
*)

open Lwt.Infix

(* externals: implemented in the C stub you compiled into the OCaml project *)
external caml_ring_open : string -> nativeint = "caml_ring_open"
external caml_ring_close : nativeint -> unit = "caml_ring_close"
(* caml_ring_read_next should return either None or Some (msg_type, payload_string) *)
external caml_ring_read_next_blocking : nativeint -> (int * string) option = "caml_ring_read_next"

let open_ring ~path =
  caml_ring_open path

let close_ring h =
  caml_ring_close h

let read_next_lwt h =
  (* run the blocking C read in a preemptive thread so the Lwt reactor is not blocked *)
  Lwt_preemptive.detach (fun () -> caml_ring_read_next_blocking h) ()
  >|= fun opt -> opt
