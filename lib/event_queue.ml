(* lib/event_queue.ml *)
open Lwt.Infix
open Event_types

type drop_policy = BlockProducer | DropOldest | DropNewest

type t = {
  q : (event Queue.t);
  capacity : int;
  policy : drop_policy;
  mutex : Lwt_mutex.t;
  not_empty : unit Lwt_condition.t;
  not_full : unit Lwt_condition.t;
}

let default_capacity = 1024
let default_policy = DropOldest

let create ?(capacity=default_capacity) ?(policy=default_policy) () =
  {
    q = Queue.create ();
    capacity;
    policy;
    mutex = Lwt_mutex.create ();
    not_empty = Lwt_condition.create ();
    not_full = Lwt_condition.create ();
  }

let capacity t = t.capacity

let size t =
  Lwt_mutex.with_lock t.mutex (fun () -> Lwt.return (Queue.length t.q))

let push t ev =
  (* Implementation strategy:
     - Grab mutex.
     - If space, push and signal.
     - If full and BlockProducer: wait on not_full and retry.
     - If full and DropOldest/DropNewest: behave accordingly. *)
  let loop () =
    Lwt_mutex.with_lock t.mutex (fun () ->
      let qlen = Queue.length t.q in
      if qlen < t.capacity then (
        Queue.add ev t.q;
        Lwt_condition.signal t.not_empty ();
        Lwt.return_true
      ) else (
        match t.policy with
        | DropNewest ->
            (* drop incoming and return immediately *)
            Lwt.return_false
        | DropOldest ->
            let _ = Queue.take t.q in
            Queue.add ev t.q;
            Lwt_condition.signal t.not_empty ();
            (* signal possible producers waiting for space *)
            Lwt_condition.signal t.not_full ();
            Lwt.return_true
        | BlockProducer ->
            (* release mutex and wait for not_full, then retry *)
            Lwt_condition.wait ~mutex:t.mutex t.not_full >>= fun () ->
            Lwt.return_true
      )
    ) >>= fun pushed ->
    if pushed then Lwt.return_unit else Lwt.return_unit
  in
  (* If BlockProducer policy, loop may wait internally; otherwise it completes quickly. *)
  (* To avoid a busy loop for BlockProducer we re-enter loop when signalled. *)
  let drive () =
    Lwt.catch
      (fun () ->
        loop () >>= fun () -> Lwt.return_unit)
      (fun ex -> Lwt.fail ex)
  in
  drive ()

let pop t =
  let rec wait_and_pop () =
    Lwt_mutex.with_lock t.mutex (fun () ->
      if Queue.is_empty t.q then
        Lwt_condition.wait ~mutex:t.mutex t.not_empty >>= fun () ->
        (* woken; try again *)
        Lwt.return_none
      else
        let ev = Queue.take t.q in
        Lwt_condition.signal t.not_full ();
        Lwt.return_some ev
    ) >>= function
    | Some ev -> Lwt.return ev
    | None -> wait_and_pop ()
  in
  wait_and_pop ()

let try_pop t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    if Queue.is_empty t.q then Lwt.return_none
    else
      let ev = Queue.take t.q in
      Lwt_condition.signal t.not_full ();
      Lwt.return_some ev
  )
