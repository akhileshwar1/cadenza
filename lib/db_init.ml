open Caqti_request.Infix
open Caqti_type.Std

let ( let* ) = Result.bind

let get_env name =
  Sys.getenv_opt name
  |> Option.to_result ~none:("Missing environment variable: " ^ name)

let get_uri () =
  let* host = get_env "PGHOST" in
  let* port = get_env "PGPORT" in
  let* db   = get_env "PGDATABASE" in
  let* user = get_env "PGUSER" in
  let* pass = get_env "PGPASSWORD" in
  Caqti_uri.of_string
    (Printf.sprintf "postgresql://%s:%s@%s:%s/%s" user pass host port db)

let connect () =
  let* uri = get_uri () in
  Caqti_lwt.connect uri

let create_orders_table =
  let query =
    "CREATE TABLE orders (
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
);" in
  Caqti_request.exec Caqti_type.unit query

let insert_orders =
  let query =
    "INSERT INTO orders (order_id, broker_order_id, tradingsymbol, exchange, quantity, price, trigger_price, side,
                         order_type, product, validity, status, strategy_name, lot, filled_quantity, filled_price)
     VALUES (
      "ord001", "broker001", "NIFTY24MAY18400CE", "NSE",
       150, 18.75, 0.0, "Buy", "Limit", "MIS", "DAY", "Pending",
       "straddle-entry", 75, 0, 0.0)" in
  Caqti_request.exec Caqti_type.unit query

let setup (module Db : Caqti_lwt.CONNECTION) =
  let* () = Db.exec create_orders_table () in
  Db.exec insert_orders ()

let teardown (module Db : Caqti_lwt.CONNECTION) =
  let drop = Caqti_request.exec Caqti_type.unit "DROP TABLE IF EXISTS orders" in
  Db.exec drop ()
