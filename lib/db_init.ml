open Caqti_request.Infix
open Order

let get_env name =
  Sys.getenv_opt name
  |> Option.to_result ~none:("Missing environment variable: " ^ name)

let get_uri () =
  let env_vars =
    let ( let* ) = Result.bind in
    let* host = get_env "PGHOST" in
    let* port = get_env "PGPORT" in
    let* db   = get_env "PGDATABASE" in
    let* user = get_env "PGUSER" in
    let* pass = get_env "PGPASSWORD" in
    Ok (user, pass, host, port, db)
  in
  match env_vars with
  | Ok (user, pass, host, port, db) ->
    (Printf.sprintf "postgresql://%s:%s@%s:%s/%s" user pass host port db)
  | Error _ -> "postgresql://"

let connect () =
  let uri = get_uri () in
  Caqti_lwt_unix.connect (Uri.of_string uri)
  |> Lwt_result.map (fun conn -> (module struct
      include (val conn : Caqti_lwt.CONNECTION)
    end : Caqti_lwt.CONNECTION))

let create_orders_table =
  Caqti_type.(unit ->. unit)
    {|
    CREATE TABLE IF NOT EXISTS orders (
    order_id TEXT PRIMARY KEY,
    broker_order_id TEXT,
    tradingsymbol TEXT,
    exchange TEXT,
    quantity INTEGER,
    price REAL,
    trigger_price REAL,
    side TEXT,
    order_type TEXT,
    product TEXT,
    validity TEXT,
    status TEXT,
    strategy_name TEXT,
    lot INTEGER,
    filled_quantity INTEGER,
    filled_price REAL
    )
    |}

let setup (module Conn : Caqti_lwt.CONNECTION) =
  let ( let* ) = Lwt_result.bind in

  (* Create sample order *)
  let sample_order = {
    tradingsymbol = "NIFTY24MAY18400CE";
    exchange = "NSE";
    quantity = 150;
    price = 18.75;
    trigger_price = 0.0;
    side = Buy;
    order_type = Limit;
    product = MIS;
    validity = DAY;
    status = Some Pending;
    filled_quantity = 100;
    filled_price = 15.0;
    strategy_name = "straddle-entry";
    lot = 75;
    order_id = "ord001";
    broker_order_id = "broker001";
  } in

  let* () = Conn.start () in
  let* () = Conn.exec create_orders_table () in
  let* () = Order_store.insert (module Conn) sample_order in
  Conn.commit ()
