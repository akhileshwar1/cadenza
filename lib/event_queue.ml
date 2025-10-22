(* lib/event_queue.ml *)
open Lwt.Infix
open Event_types

(* drop_policy is no longer needed at the top level, as it's implied for each queue *)
type t = {
  critical_q : event Queue.t;
  market_q : event Queue.t;
  critical_capacity : int;
  market_capacity : int;
  mutex : Lwt_mutex.t;
  (* Conditions for when each queue becomes non-empty (for popping) *)
  not_empty_critical : unit Lwt_condition.t;
  not_empty_market : unit Lwt_condition.t;
  (* Condition for when the critical queue has space (for blocking push) *)
  not_full_critical : unit Lwt_condition.t;
}

let default_critical_capacity = 100
let default_market_capacity = 1024

let create ?(critical_capacity=default_critical_capacity) ?(market_capacity=default_market_capacity) () =
  {
    critical_q = Queue.create ();
    market_q = Queue.create ();
    critical_capacity;
    market_capacity;
    mutex = Lwt_mutex.create ();
    not_empty_critical = Lwt_condition.create ();
    not_empty_market = Lwt_condition.create ();
    not_full_critical = Lwt_condition.create ();
  }

let capacity t = t.critical_capacity + t.market_capacity (* now returns total capacity *)

let size t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    Lwt.return (Queue.length t.critical_q + Queue.length t.market_q)
  )

let is_critical_event ev =
  match ev.Event_types.typ with
  | Event_types.OMS_UPDATE
  | Event_types.SNAPSHOT -> true
  | _ -> false

let push t ev =
  let is_critical = is_critical_event ev in

  if is_critical then
    (* Critical Push (BlockProducer) - Logic is moved to an external loop *)
    let rec critical_push_loop () =
      Lwt_mutex.with_lock t.mutex (fun () ->
        let qlen = Queue.length t.critical_q in
        if qlen < t.critical_capacity then (
          (* SUCCESS: Push and signal *)
          Queue.add ev t.critical_q;
          Lwt_condition.signal t.not_empty_critical ();
          Lwt.return `Pushed
        ) else (
          (* BLOCKED: Wait for signal, then retry loop *)
          Printf.eprintf "[EventQueue] Critical queue full (%d). Blocking producer.\n%!" t.critical_capacity;
          Lwt_condition.wait ~mutex:t.mutex t.not_full_critical >>= fun () ->
          Lwt.return `Retry_Wait (* <--- CRITICAL CHANGE: Return flag instead of recursion *)
        )
      ) >>= function
      | `Pushed -> Lwt.return_unit
      | `Retry_Wait -> critical_push_loop () (* <--- Loop externally after lock is released *)
    in
    critical_push_loop ()
  else
    (* Market Push (DropOldest) - Non-blocking and remains inside a single with_lock call *)
    Lwt_mutex.with_lock t.mutex (fun () ->
      let qlen = Queue.length t.market_q in
      if qlen < t.market_capacity then (
        Queue.add ev t.market_q;
        Lwt_condition.signal t.not_empty_market ();
        Lwt.return_unit
      ) else (
        (* Drop oldest market data and add new *)
        let _ = Queue.take t.market_q in
        Queue.add ev t.market_q;
        Lwt.return_unit
      )
    )
  
(* POP: Corrected to use external recursion and ensure Lwt.choose runs without the mutex held *)
let pop t =
  let rec wait_and_pop () =
    (* Step 1: Acquire lock ONLY to check queues and decide on action *)
    Lwt_mutex.with_lock t.mutex (fun () ->
      (* A. Critical Queue: Highest Priority *)
      if not (Queue.is_empty t.critical_q) then (
        let ev = Queue.take t.critical_q in
        Lwt_condition.signal t.not_full_critical (); 
        Lwt.return (`Found ev) (* Signal event found, release lock *)
      )
      (* B. Market Queue: Second Priority *)
      else if not (Queue.is_empty t.market_q) then (
        let ev = Queue.take t.market_q in
        Lwt.return (`Found ev) (* Signal event found, release lock *)
      )
      (* C. Both Empty: Signal need to wait *)
      else (
        Lwt.return `Waiting (* Signal wait, release lock *)
      )
    ) >>= function
    | `Found ev -> Lwt.return ev (* Event popped successfully *)
    | `Waiting ->
      (* Step 2: Lock is released. Wait for a signal outside the lock. *)
      Lwt.choose [
        Lwt_condition.wait t.not_empty_critical; (* Standard Lwt condition wait *)
        Lwt_condition.wait t.not_empty_market;
      ] >>= fun () ->
      (* Step 3: Woken up. Lock is NOT held. Retry loop to re-acquire lock and check queues. *)
      wait_and_pop ()
  in
  wait_and_pop ()

let try_pop t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    if not (Queue.is_empty t.critical_q) then (
      let ev = Queue.take t.critical_q in
      Lwt_condition.signal t.not_full_critical ();
      Lwt.return_some ev
    )
    else if not (Queue.is_empty t.market_q) then (
      let ev = Queue.take t.market_q in
      Lwt.return_some ev
    )
    else Lwt.return_none
  )
