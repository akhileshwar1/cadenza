open Bb_strategy

val insert :
  (module Caqti_lwt.CONNECTION) ->
  candle ->
  (unit, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

val count :
  (module Caqti_lwt.CONNECTION) ->
  (int, [> Caqti_error.call_or_retrieve ]) Lwt_result.t
