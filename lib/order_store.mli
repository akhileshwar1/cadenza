
val insert :
  (module Caqti_lwt.CONNECTION) ->
  Order.t ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

(** Updates an existing order in the database using its [order_id] as key. *)
val update :
  (module Caqti_lwt.CONNECTION) ->
  Order.t ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t
