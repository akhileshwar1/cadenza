(* orderbook.ml *)

type price = int64
type size_ = int64

(* let price_scale = Int64.of_int 100000000 *)

module PriceDesc = struct
  type t = price
  let compare = Int64.compare |> (fun cmp a b -> cmp b a)
end
module Bids = Map.Make(PriceDesc)

module PriceAsc = struct
  type t = price
  let compare = Int64.compare
end
module Asks = Map.Make(PriceAsc)

type t = {
  mutable bids : size_ Bids.t;
  mutable asks : size_ Asks.t;
  mutable last_update_id : int64;
}

let create () = { bids = Bids.empty; asks = Asks.empty; last_update_id = 0L }

let clear ob =
  ob.bids <- Bids.empty;
  ob.asks <- Asks.empty;
  ob.last_update_id <- 0L

let parse_price s =
  let f = float_of_string s in
  Int64.of_float (floor (f *. 1e8 +. 0.5))

let parse_size s =
  let f = float_of_string s in
  Int64.of_float (floor (f *. 1e8 +. 0.5))

let set_from_snapshot (ob : t) (snap : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  ob.bids <- Bids.empty;
  ob.asks <- Asks.empty;
  let last = snap |> member "lastUpdateId" |> to_int |> Int64.of_int in
  ob.last_update_id <- last;
  let bids = snap |> member "bids" |> to_list in
  List.iter (fun elt ->
    match elt with
    | `List [`String p; `String q] ->
        let pp = parse_price p in
        let qq = parse_size q in
        if qq > 0L then ob.bids <- Bids.add pp qq ob.bids
    | _ -> ()
    ) bids;
  let asks = snap |> member "asks" |> to_list in
  List.iter (fun elt ->
    match elt with
    | `List [`String p; `String q] ->
        let pp = parse_price p in
        let qq = parse_size q in
        if qq > 0L then ob.asks <- Asks.add pp qq ob.asks
    | _ -> ()
    ) asks

let apply_event (ob : t) (ev : Yojson.Safe.t) : bool =
  let open Yojson.Safe.Util in
  let last_u = ev |> member "u" |> to_int |> Int64.of_int in
  let first_U = ev |> member "U" |> to_int |> Int64.of_int in
  if Int64.compare last_u ob.last_update_id < 0 then
    true
  else
    let expected_next = Int64.add ob.last_update_id 1L in
    if Int64.compare first_U expected_next > 0 then
      false
  else (
    (* apply bids *)
    (try
      let bids = ev |> member "b" |> to_list in
      List.iter (fun lvl ->
        match lvl with
           | `List [`String p; `String q] ->
               let pp = parse_price p in
               let qq = parse_size q in
               if qq = 0L then ob.bids <- Bids.remove pp ob.bids
               else ob.bids <- Bids.add pp qq ob.bids
           | _ -> ()
           ) bids
        with _ -> ());
      (* apply asks *)
      (try
        let asks = ev |> member "a" |> to_list in
        List.iter (fun lvl ->
          match lvl with
           | `List [`String p; `String q] ->
               let pp = parse_price p in
               let qq = parse_size q in
               if qq = 0L then ob.asks <- Asks.remove pp ob.asks
               else ob.asks <- Asks.add pp qq ob.asks
           | _ -> ()
           ) asks
          with _ -> ());
      ob.last_update_id <- last_u;
      true
      )

let format_price p =
  let f = (Int64.to_float p) /. 1e8 in
  Printf.sprintf "%.8f" f

let format_size q =
  let f = (Int64.to_float q) /. 1e8 in
  Printf.sprintf "%.8f" f

let print_top ?(n=5) (ob : t) =
  Printf.printf "OrderBook last_update_id=%Ld\n" ob.last_update_id;
  Printf.printf " Asks (lowest):\n";
  let cnt = ref 0 in
  Asks.iter (fun price qty ->
    if !cnt < n then begin
      Printf.printf "  %s : %s\n" (format_price price) (format_size qty);
      incr cnt
end
      ) ob.asks;
  Printf.printf " Bids (highest):\n";
  let cnt2 = ref 0 in
  Bids.iter (fun price qty ->
    if !cnt2 < n then begin
      Printf.printf "  %s : %s\n" (format_price price) (format_size qty);
      incr cnt2
    end
      ) ob.bids;
  flush stdout

let total_levels ob = (Bids.cardinal ob.bids) + (Asks.cardinal ob.asks)
let last_update_id ob = ob.last_update_id
