(* lib/position.ml *)

type status =
  | Open
  | Closed

type side =
  | Buy
  | Sell

type t = {
  opened_at_epoch : float;
  closed_at_epoch : float;
  symbol : string;
  buy_qty : int;
  sell_qty: int;
  net_buy_price : float;
  net_sell_price : float;
  net_qty : float;
  net_price : float;
  side : side;
  value : float;
  status : status;
}

let update_or_insert_position (positions : t list) (order : Order.t) : t list =
  let symbol = order.tradingsymbol in
  let qty = order.quantity in
  let price = order.price in
  let side = order.side in
  let now = Unix.gettimeofday () in

  let rec update_positions acc = function
    | [] ->
      let new_position =
        match side with
        | Buy ->
          {
            opened_at_epoch = now;
            closed_at_epoch = 0.0;
            symbol;
            buy_qty = qty;
            sell_qty = 0;
            net_buy_price = price;
            net_sell_price = 0.0;
            side = Buy;
            net_qty = float_of_int qty;
            net_price = price;
            value = float_of_int qty *. price;
            status = Open;
          }
        | Sell ->
          {
            opened_at_epoch = now;
            closed_at_epoch = 0.0;
            symbol;
            buy_qty = 0;
            sell_qty = -qty;
            net_buy_price = 0.0;
            net_sell_price = price;
            net_price = price;
            side = Sell;
            net_qty = -. (float_of_int qty);
            value = float_of_int qty *. price;
            status = Open;
          }
      in
      List.rev (new_position :: acc)

    | pos :: rest when pos.symbol = symbol ->
      let updated_pos =
        match side with
        | Buy ->
          let total_qty = int_of_float pos.net_qty + qty in
          let total_cost = (float_of_int pos.buy_qty *. pos.net_buy_price) +. (float_of_int qty *. price) in
          let new_buy_price = total_cost /. float_of_int (pos.buy_qty + qty) in
          let net_price = (new_buy_price +. pos.net_sell_price) /. 2.0 in
          let side = if total_qty > 0 then Buy else Sell in
          { pos with
            buy_qty = pos.buy_qty + qty;
            net_qty = float_of_int total_qty;
            net_buy_price = new_buy_price;
            net_price = net_price;
            value = float_of_int total_qty *. net_price;
            side = side;
          }
        | Sell ->
          let total_qty = int_of_float pos.net_qty - qty in
          let total_cost = (float_of_int pos.sell_qty *. pos.net_sell_price) +. (float_of_int qty *. price) in
          let new_sell_price = -. total_cost /. float_of_int (pos.sell_qty - qty) in
          let net_price = (new_sell_price +. pos.net_buy_price) /. 2.0 in
          let side = if total_qty > 0 then Buy else Sell in
          { pos with
            sell_qty = pos.sell_qty - qty;
            net_qty = float_of_int total_qty;
            net_sell_price = new_sell_price;
            net_price = net_price;
            value = float_of_int total_qty *. net_price;
            side = side;
          }
      in

      let updated_pos =
        if updated_pos.buy_qty = -updated_pos.sell_qty && updated_pos.buy_qty > 0 then
          { updated_pos with status = Closed; closed_at_epoch = now }
        else
          updated_pos
      in

      List.rev_append acc (updated_pos :: rest)

    | pos :: rest ->
      update_positions (pos :: acc) rest
  in

  update_positions [] positions


let update_positions_with_option_chain
  (option_chain : Option_chain.t)
  (positions : t list)
  : t list =

  let find_option_data symbol =
    let rec search = function
      | [] -> None
      | (_expiry, strikes) :: rest ->
        let rec find_in_strikes = function
          | [] -> search rest
          | (_strike_price, options) :: rest_opts ->
            match List.find_opt (fun (_key, (data : Option_chain.option_data)) -> data.symbol = symbol) options with
            | Some (_, data) -> Some data
            | None -> find_in_strikes rest_opts
        in
        find_in_strikes strikes
    in
    search option_chain
  in

  List.map
    (fun pos ->
      match find_option_data pos.symbol with
      | Some data ->
        let value = pos.net_qty *. data.ltp in
        {
          pos with
          value;
        }
      | None ->
        (* Option data not found — return unchanged or log warning *)
        pos
    )
    positions
