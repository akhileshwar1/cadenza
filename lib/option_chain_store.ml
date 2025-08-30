open Option_chain

let ( let* ) = Lwt_result.bind
(* Define one row in the option chain DB table *)
type row = {
  timestamp : Ptime.t;
  expiry : string;
  strike : float;
  option_type : string;  (* "CE" or "PE" *)
  symbol : string;
  ltp : float;
  delta : float;
  iv : float;
  vega : float;
  theta : float;
  gamma : float;
  rho : float;
}

(* Flatten a full option chain snapshot into a list of DB rows *)
let flatten (chain : Option_chain.t) : row list =
  List.concat_map (fun (expiry, strikes) ->
    List.concat_map (fun (strike, options) ->
      List.map (fun (option_type, data : string * option_data) ->
        {
          timestamp = data.timestamp;
          expiry;
          strike;
          option_type;
          symbol = data.symbol;
          ltp = data.ltp;
          delta = data.delta;
          iv = data.iv;
          vega = data.vega;
          theta = data.theta;
          gamma = data.gamma;
          rho = data.rho;
        }
      ) options
    ) strikes
  ) chain

module Q = struct
  open Caqti_request.Infix

  (* Caqti type for one row *)
  let option_chain_row =
    let open Caqti_type in
    t12 ptime string float string string float float float float float float float

  (* Insert query *)
  let insert =
    Caqti_type.(option_chain_row ->. unit)
      {|
      INSERT INTO option_chain (
      timestamp, expiry, strike, option_type, symbol,
      ltp, delta, iv, vega, theta, gamma, rho
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |}
end

(* Convert one row to a tuple for Caqti *)
let to_db_tuple (r : row) =
  ( r.timestamp, r.expiry, r.strike, r.option_type, r.symbol,
    r.ltp, r.delta, r.iv, r.vega, r.theta, r.gamma, r.rho )

let rec iter_result_s f = function
  | [] -> Lwt_result.return ()
  | x :: xs ->
    let* () = f x in
    iter_result_s f xs

(* Insert a full option chain snapshot *)
let insert (module Conn : Caqti_lwt.CONNECTION) (chain : Option_chain.t) =
  let rows = flatten chain in
  let* () =
    iter_result_s
      (fun row -> Conn.exec Q.insert (to_db_tuple row))
      rows
  in
  Conn.commit ()
