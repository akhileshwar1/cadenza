open Order

let ( let* ) = Lwt_result.bind

module Q = struct
  open Caqti_request.Infix
  (* Code to decode db rows into order, useful in get queries *)
  (* let order = *)
  (*   let intro *)
  (*     tradingsymbol *)
  (*     exchange *)
  (*     quantity *)
  (*     price *)
  (*     trigger_price *)
  (*     side *)
  (*     order_type *)
  (*     product *)
  (*     validity *)
  (*     status *)
  (*     strategy_name *)
  (*     lot *)
  (*     filled_quantity *)
  (*     filled_price *)
  (*     order_id *)
  (*     broker_order_id = *)
  (*     { *)
  (*       tradingsymbol; *)
  (*       exchange; *)
  (*       quantity; *)
  (*       price; *)
  (*       trigger_price; *)
  (*       side = if side = "Buy" then Buy else Sell; *)
  (*       order_type = if order_type = "Limit" then Limit else Market; *)
  (*       product = *)
  (*         (match product with *)
  (*           | "CNC" -> CNC *)
  (*           | "NRML" -> NRML *)
  (*           | _ -> MIS); *)
  (*       validity = if validity = "DAY" then DAY else IOC; *)
  (*       status = Some (string_to_status status); *)
  (*       strategy_name; *)
  (*       lot; *)
  (*       filled_quantity; *)
  (*       filled_price; *)
  (*       order_id; *)
  (*       broker_order_id; *)
  (*     } *)
  (*   in *)
  (*   let proj_string_of_enum = function *)
  (*     | Buy -> "Buy" *)
  (*     | Sell -> "Sell" *)
  (*   in *)
  (*   let proj_order_type = function Limit -> "Limit" | Market -> "Market" in *)
  (*   let proj_product = function CNC -> "CNC" | NRML -> "NRML" | MIS -> "MIS" in *)
  (*   let proj_validity = function DAY -> "DAY" | IOC -> "IOC" in *)
  (*   let proj_status = function Some s -> status_to_string s | None -> "Unknown" in *)

  (*   Caqti_type.Std.( *)
  (*   product intro *)
  (*   @@ proj string (fun x -> x.tradingsymbol) *)
  (*   @@ proj string (fun x -> x.exchange) *)
  (*   @@ proj int (fun x -> x.quantity) *)
  (*   @@ proj float (fun x -> x.price) *)
  (*   @@ proj float (fun x -> x.trigger_price) *)
  (*   @@ proj string (fun x -> proj_string_of_enum x.side) *)
  (*   @@ proj string (fun x -> proj_order_type x.order_type) *)
  (*   @@ proj string (fun x -> proj_product x.product) *)
  (*   @@ proj string (fun x -> proj_validity x.validity) *)
  (*   @@ proj string (fun x -> proj_status x.status) *)
  (*   @@ proj string (fun x -> x.strategy_name) *)
  (*   @@ proj int (fun x -> x.lot) *)
  (*   @@ proj int (fun x -> x.filled_quantity) *)
  (*   @@ proj float (fun x -> x.filled_price) *)
  (*   @@ proj string (fun x -> x.order_id) *)
  (*   @@ proj string (fun x -> x.broker_order_id) *)
  (*   @@ proj_end) *)

  let order_insert_type =
    let open Caqti_type in
    t2 string (
      t2 string (
        t2 int (
          t2 float (
            t2 float (
              t2 string (
                t2 string (
                  t2 string (
                    t2 string (
                      t2 string (
                        t2 string (
                          t2 int (
                            t2 int (
                              t2 float (
                                t2 string string
                              ))))))))))))))
  let insert =
    Caqti_type.(order_insert_type ->. unit)
      {|
      INSERT INTO orders (
      tradingsymbol, exchange, quantity, price, trigger_price,
      side, order_type, product, validity, status,
      strategy_name, lot, filled_quantity, filled_price,
      order_id, broker_order_id
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |}

  (* let insert' = *)
  (*   Caqti_type.(order_insert_type ->. unit) *)
  (*     {| *)
  (*     INSERT INTO orders ( *)
  (*     tradingsymbol, exchange, quantity, price, *)
  (*     trigger_price,side, order_type, product, validity, status, *)
  (*     strategy_name, lot, filled_quantity, filled_price, *)
  (*     order_id, broker_order_id *)
  (*     ) *)
  (*     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) *)
  (*     RETURNING id *)
  (*     |} *)

  let update =
    Caqti_type.(order_insert_type ->. unit)
      {|
      UPDATE orders SET
      tradingsymbol = ?, exchange = ?, quantity = ?, price = ?, trigger_price = ?,
      side = ?, order_type = ?, product = ?, validity = ?, status = ?,
      strategy_name = ?, lot = ?, filled_quantity = ?, filled_price = ?,
      broker_order_id = ?
      WHERE order_id = ?
      |}

  (* let count = *)
  (*   Caqti_type.(unit ->! int) *)
  (*     {| *)
  (*     SELECT COUNT(*) FROM orders *)
  (*     |} *)

  (* let delete_by_order_id = *)
  (*   Caqti_type.(string ->. unit) *)
  (*     {| *)
  (*     DELETE FROM orders WHERE order_id = ? *)
  (*     |} *)
end

let string_of_order_type = function
  | Limit -> "Limit"
  | Market -> "Market"

let string_of_product_type = function
  | MIS -> "MIS"
  | CNC -> "CNC"
  | NRML -> "NRML"

let string_of_validity = function
  | DAY -> "DAY"
  | IOC -> "IOC"

let string_of_side = function
  | Buy -> "Buy"
  | Sell -> "Sell"

let to_db_tuple order =
  (order.tradingsymbol,
    (order.exchange,
      (order.quantity,
        (order.price,
          (order.trigger_price,
            (string_of_side order.side,
              (string_of_order_type order.order_type,
                (string_of_product_type order.product,
                  (string_of_validity order.validity,
                    (Order.status_to_string (Option.get order.status),
                      (order.strategy_name,
                        (order.lot,
                          (order.filled_quantity,
                            (order.filled_price,
                              (order.order_id, order.broker_order_id)
                            ))))))))))))))

let insert (module Conn : Caqti_lwt.CONNECTION) (order : Order.t) =
  let* () = Conn.exec Q.insert (to_db_tuple order) in
  Conn.commit ()

let update (module Conn : Caqti_lwt.CONNECTION) (order : Order.t) =
  let* () = Conn.exec Q.update (to_db_tuple order) in
  Conn.commit ()
