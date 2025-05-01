open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe.Util

type option_data = {
  symbol : string;
  ltp : float;
  delta : float;
  bid : float;
  ask : float;
}

type t = (string * (float * (string * option_data) list) list) list

let parse_option_data json : option_data =
  {
    symbol = json |> member "symbol" |> to_string;
    ltp = json |> member "ltp" |> to_float;
    delta = json |> member "delta" |> to_float;
    bid = json |> member "bid" |> to_float;
    ask = json |> member "ask" |> to_float;
  }

let parse json : t =
  json
  |> to_assoc
  |> List.map (fun (expiry, strikes_json) ->
      let strikes =
        strikes_json
        |> to_assoc
        |> List.map (fun (strike_str, contracts_json) ->
            let strike = float_of_string strike_str in
            let contracts =
              contracts_json
              |> to_assoc
              |> List.map (fun (sym, data) ->
                  (sym, parse_option_data data))
            in
            (strike, contracts)
          )
      in
      (expiry, strikes)
    )

let get () : t Lwt.t =
  let uri = Uri.of_string "http://localhost:8000/option-chain" in
  Client.get uri >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
  let json = Yojson.Safe.from_string body_str in
  parse json
