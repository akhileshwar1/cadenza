open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Safe.Util

type option_data = {
  symbol : string;
  ltp : float;
  delta : float;
  strike : string;
  iv : float;
  vega : float;
  theta : float;
  gamma : float;
  rho : float;
}

type t = (string * (float * (string * option_data) list) list) list

(* let log msg = Printf.printf "[LOG] %s\n%!" msg *)

let safe_to_string key json =
  match json |> member key with
  | `String s -> s
  | `Null -> (* log (Printf.sprintf "Key '%s' is null" key); *) ""
  | _ -> (* log (Printf.sprintf "Key '%s' is not a string" key); *) ""

let safe_to_float key json =
  match json |> member key with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Null -> (* log (Printf.sprintf "Key '%s' is null" key); *) nan
  | _ -> (* log (Printf.sprintf "Key '%s' is not a float" key); *) nan

let parse_option_data json : option_data =
  let symbol = safe_to_string "symbol" json in
  let ltp = safe_to_float "ltp" json in
  let delta = safe_to_float "delta" json in
  let iv = safe_to_float "IV" json in
  let vega = safe_to_float "vega" json in
  let theta = safe_to_float "theta" json in
  let gamma = safe_to_float "gamma" json in
  let rho = safe_to_float "rho" json in
  let strike = safe_to_string "strike" json in
  (* log (Printf.sprintf "Parsed option: %s ltp=%f delta=%f %s" symbol ltp delta strike); *)
  {
    symbol;
    ltp;
    delta;
    strike;
    iv;
    vega;
    theta;
    gamma;
    rho;
  }

let parse (json : Yojson.Safe.t) : t =
  (* log "Starting parse"; *)
  json
  |> to_assoc
  |> List.map (fun (expiry, strikes_json) ->
    (* log (Printf.sprintf "Parsing expiry: %s" expiry); *)
    let strikes =
      strikes_json
      |> to_assoc
      |> List.map (fun (strike_str, contracts_json) ->
        (* log (Printf.sprintf "  Strike: %s" strike_str); *)
        let strike =
          try float_of_string strike_str
          with Failure _ ->
            (* log (Printf.sprintf "  Failed to convert strike '%s' to float" strike_str); *)
            nan
        in
        let contracts =
          contracts_json
          |> to_assoc
          |> List.map (fun (sym, data) ->
            (* log (Printf.sprintf "    Contract: %s" sym); *)
            (sym, parse_option_data data))
        in
        (strike, contracts)
      )
    in
    (expiry, strikes)
  )

(* logic to roll to next expiry on the expiry day/thursdays *)
let find_next_expiry expiry_data today =
  match expiry_data with
  | [] -> None
  | hd :: tl ->
    let date = safe_to_string "date" hd in
    if date = today then
      (match tl with
        | next :: _ -> Some (safe_to_string "expiry" next)
        | [] -> None)
    else
      Some (safe_to_string "expiry" hd)

let uri_with_expiry expiry =
  Uri.of_string (Printf.sprintf "http://localhost:8000/option-chain?symbol=NSE:NIFTY50-INDEX&expiry=%s" expiry)

let get () : t Lwt.t =
  let base_uri = Uri.of_string "http://localhost:8000/option-chain?symbol=NSE:NIFTY50-INDEX" in
  Client.get base_uri >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  let json = Yojson.Safe.from_string body_str in
  let expiry_data = json |> member "expiryData" |> to_list in
  let chain = json |> member "chain" in
  let today =
    let open Unix in
    let tm = localtime (time ()) in
    Printf.sprintf "%02d-%02d-%04d" tm.tm_mday (tm.tm_mon + 1) (tm.tm_year + 1900)
  in
  let next_expiry_opt = find_next_expiry expiry_data today in
  match next_expiry_opt with
  | Some expiry_epoch when expiry_epoch <> "" ->
    (* Make second request with the next expiry in epoch*)
    let uri = uri_with_expiry expiry_epoch in
    Client.get uri >>= fun (_, body2) ->
    Cohttp_lwt.Body.to_string body2 >|= fun body2_str ->
    let json2 = Yojson.Safe.from_string body2_str in
    let chain2 = json2 |> member "chain" in
    parse chain2 
  | _ ->
    (* Use original *)
    Lwt.return (parse chain)
