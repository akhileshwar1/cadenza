open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Safe.Util

type option_data = {
  symbol : string;
  ltp : float;
  delta : float;
  strike : string;
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
  let strike = safe_to_string "strike" json in
  (* log (Printf.sprintf "Parsed option: %s ltp=%f delta=%f %s" symbol ltp delta strike); *)
  {
    symbol;
    ltp;
    delta;
    strike;
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


let get () : t Lwt.t =
  let uri = Uri.of_string "http://localhost:8000/option-chain?symbol=NSE:NIFTY50-INDEX" in
  Client.get uri >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
  (* Printf.printf "body is %s\n%!" body_str; *)
  let json = Yojson.Safe.from_string body_str in
  parse json
