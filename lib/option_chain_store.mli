val insert :
  (module Caqti_lwt.CONNECTION) ->
  Option_chain.t ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t
