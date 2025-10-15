(* orderbook.mli *)

(** A tiny L2 order-book module. Prices and sizes are stored as int64 ticks
    using a fixed PRICE_SCALE (1e8) to match the C++ producer. *)

type t

val create : unit -> t
val clear : t -> unit

(** Build orderbook from snapshot JSON (Yojson.Safe.t) which must contain:
  { "lastUpdateId": N, "bids": [["p","q"], ...], "asks": [...] } *)
val set_from_snapshot : t -> Yojson.Safe.t -> unit

(** Apply a depthUpdate JSON event (Binance format). Returns:
  - true on success (applied or ignored old event)
    - false if a gap is detected (consumer should resync). *)
val apply_event : t -> Yojson.Safe.t -> bool

val print_top : ?n:int -> t -> unit

val total_levels : t -> int
val last_update_id : t -> int64

val top_n : ?n:int -> t -> (float * float) list * (float * float) list
val symbol_of : t -> string 
