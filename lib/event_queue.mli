(* lib/event_queue.mli *)
open Event_types

type drop_policy = BlockProducer | DropOldest | DropNewest

type t

val create : ?capacity:int -> ?policy:drop_policy -> unit -> t

val push : t -> event -> unit Lwt.t
(** Push an event. Behavior depends on policy:
    - BlockProducer: [push] blocks until space is available.
    - DropOldest: if full, oldest item is dropped and new item inserted.
    - DropNewest: if full, new item is dropped (operation returns immediately). *)

val pop : t -> event Lwt.t
val try_pop : t -> event option Lwt.t

val size : t -> int Lwt.t
val capacity : t -> int
