open Order

module Q = struct
  open Caqti_request.Infix
  let order =
    let intro
      tradingsymbol
      exchange
      quantity
      price
      trigger_price
      side
      order_type
      product
      validity
      status
      strategy_name
      lot
      filled_quantity
      filled_price
      order_id
      broker_order_id =
      {
        tradingsymbol;
        exchange;
        quantity;
        price;
        trigger_price;
        side = if side = "Buy" then Buy else Sell;
        order_type = if order_type = "Limit" then Limit else Market;
        product =
          (match product with
            | "CNC" -> CNC
            | "NRML" -> NRML
            | _ -> MIS);
        validity = if validity = "DAY" then DAY else IOC;
        status = Some (string_to_status status);
        strategy_name;
        lot;
        filled_quantity;
        filled_price;
        order_id;
        broker_order_id;
      }
    in
    let proj_string_of_enum = function
      | Buy -> "Buy"
      | Sell -> "Sell"
    in
    let proj_order_type = function Limit -> "Limit" | Market -> "Market" in
    let proj_product = function CNC -> "CNC" | NRML -> "NRML" | MIS -> "MIS" in
    let proj_validity = function DAY -> "DAY" | IOC -> "IOC" in
    let proj_status = function Some s -> status_to_string s | None -> "Unknown" in

    Caqti_type.Std.(
    product intro
    @@ proj string (fun x -> x.tradingsymbol)
    @@ proj string (fun x -> x.exchange)
    @@ proj int (fun x -> x.quantity)
    @@ proj float (fun x -> x.price)
    @@ proj float (fun x -> x.trigger_price)
    @@ proj string (fun x -> proj_string_of_enum x.side)
    @@ proj string (fun x -> proj_order_type x.order_type)
    @@ proj string (fun x -> proj_product x.product)
    @@ proj string (fun x -> proj_validity x.validity)
    @@ proj string (fun x -> proj_status x.status)
    @@ proj string (fun x -> x.strategy_name)
    @@ proj int (fun x -> x.lot)
    @@ proj int (fun x -> x.filled_quantity)
    @@ proj float (fun x -> x.filled_price)
    @@ proj string (fun x -> x.order_id)
    @@ proj string (fun x -> x.broker_order_id)
    @@ proj_end)

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
      INSERT INTO $.order (
      tradingsymbol, exchange, quantity, price,
      side, status,
      strategy_name, lot, filled_quantity, filled_price,
      order_id, broker_order_id
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |}

  let insert' =
    Caqti_type.(order_insert_type ->. unit)
      {|
      INSERT INTO $.order (
      tradingsymbol, exchange, quantity, price,
      side, status,
      strategy_name, lot, filled_quantity, filled_price,
      order_id, broker_order_id
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      RETURNING id
      |}

  let count =
    Caqti_type.(unit ->! int)
      {|
      SELECT COUNT(*) FROM $.order
      |}

  let delete_by_order_id =
    Caqti_type.(string ->. unit)
      {|
      DELETE FROM $.order WHERE order_id = ?
      |}
end
