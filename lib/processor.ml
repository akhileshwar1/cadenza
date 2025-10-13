(* lib/processor.ml *)
open Lwt.Infix
open Event_types

type 'ls handler = 'ls Strategy.state -> event -> 'ls Strategy.state Lwt.t

let start ~queue ~init_state ~handler ~stop_ref () =
  let state_mutex = Lwt_mutex.create () in
  let state_ref = ref init_state in

  let rec loop () =
    if !stop_ref then Lwt.return_unit
    else
      Event_queue.pop queue >>= fun ev ->
      Lwt_mutex.with_lock state_mutex (fun () ->
        handler !state_ref ev >>= fun new_state ->
        state_ref := new_state;
        Lwt.return_unit
      ) >>= fun () ->
      loop ()
  in

  let promise =
    Lwt.async (fun () -> loop ());
    (* return a promise that resolves when stop_ref becomes true; we poll lightly *)
    let rec waiter () =
      if !stop_ref then Lwt.return !state_ref
      else Lwt_unix.sleep 0.05 >>= waiter
    in
    waiter ()
  in
  promise
