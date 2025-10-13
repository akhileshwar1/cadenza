(* lib/processor.mli *)
open Event_types

type 'ls handler = 'ls Strategy.state -> event -> 'ls Strategy.state Lwt.t

val start :
  queue:Event_queue.t ->
  init_state:'ls Strategy.state ->
  handler:('ls) handler ->
  stop_ref:bool ref ->
  unit ->
  ('ls Strategy.state) Lwt.t
(** Start processor loop: returns a promise that resolves to the final state when stopped. *)
