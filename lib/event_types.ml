(* lib/event_types.ml *)
(** Event type definitions shared across modules. *)

type msg_type =
  | DEPTH_UPDATE
  | SNAPSHOT
  | OMS_UPDATE
  | TICK
  | CUSTOM of int

type event = {
  typ : msg_type;
  payload : Yojson.Safe.t;
  recv_at : Ptime.t;
}
