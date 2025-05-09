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
  net_qty : int;
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
      Printf.printf "Adding new position for symbol %s with price %f and qty %d \n%!" symbol price qty;
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
            net_qty = qty;
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
            net_qty = - qty;
            value = -.float_of_int qty *. price;
            status = Open;
          }
      in
      List.rev (new_position :: acc)

    | pos :: rest when pos.symbol = symbol ->
      let updated_pos =
        match side with
        | Buy ->
          let total_qty = pos.net_qty + qty in
          let total_buy_cost = (float_of_int pos.buy_qty *. pos.net_buy_price) +. (float_of_int qty *. price) in
          let total_sell_cost = (float_of_int pos.sell_qty *. pos.net_sell_price) in
          let new_buy_price = total_buy_cost /. float_of_int (pos.buy_qty + qty) in
          let net_price, value =
            if total_qty = 0 then
              (0.0, total_buy_cost +. total_sell_cost)
            else
              let net_price = (total_sell_cost +. total_buy_cost) /. float_of_int total_qty in
              (net_price, float_of_int total_qty *. net_price)
          in
          let side = if total_qty > 0 then Buy else Sell in
          Printf.printf
            "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2d\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n%!"
            symbol
            price
            pos.net_price
            net_price
            total_qty
            "BUY"
            pos.value
            value;
          { pos with
            buy_qty = pos.buy_qty + qty;
            net_qty = total_qty;
            net_buy_price = new_buy_price;
            net_price = net_price;
            value = value; 
            side = side;
          }
        | Sell ->
          let total_qty = pos.net_qty - qty in
          let total_sell_cost = (float_of_int pos.sell_qty *. pos.net_sell_price) +. (-.float_of_int qty *. price) in
          let total_buy_cost = (float_of_int pos.buy_qty *. pos.net_buy_price) in
          let new_sell_price =  total_sell_cost /. float_of_int (pos.sell_qty - qty) in
          let net_price, value =
            if total_qty = 0 then
              (0.0, total_buy_cost +. total_sell_cost)
            else
              let net_price = (total_sell_cost +. total_buy_cost) /. float_of_int total_qty in
              (net_price, float_of_int total_qty *. net_price)
          in
          let side = if total_qty > 0 then Buy else Sell in
          Printf.printf
            "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2d\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n%!"
            symbol
            price
            pos.net_price
            net_price
            total_qty
            "SELL"
            pos.value
            value;
          { pos with
            sell_qty = pos.sell_qty - qty;
            net_qty = total_qty;
            net_sell_price = new_sell_price;
            net_price = net_price;
            value = value;
            side = side;
          }
      in

      let updated_pos =
        if updated_pos.net_qty = 0 && updated_pos.buy_qty > 0 then
          { updated_pos with status = Closed; closed_at_epoch = now }
        else
          updated_pos
      in

      List.rev_append acc (updated_pos :: rest)

    | pos :: rest ->
      update_positions (pos :: acc) rest
  in

  update_positions [] positions

(*NOTE: only works for NIFTY15MAY23800PE*)
let extract_strike symbol =
  let len = String.length symbol in
  if len > 5 then
    String.sub symbol (len - 7) 7  (* "23800PE" is 7 characters *)
  else
    failwith ("Invalid symbol: " ^ symbol)

let find_option_data strike option_chain =
    let rec search = function
      | [] -> None
      | (_expiry, strikes) :: rest ->
        let rec find_in_strikes = function
          | [] -> search rest
          | (_strike_price, options) :: rest_opts ->
            match List.find_opt (fun (_key, (data : Option_chain.option_data)) -> data.strike = strike) options with
            | Some (_, data) -> Some data
            | None -> find_in_strikes rest_opts
        in
        find_in_strikes strikes
    in
    search option_chain

let update_positions_with_option_chain
  (option_chain : Option_chain.t)
  (positions : t list)
  : t list =
  List.map
    (fun pos ->
      match find_option_data (extract_strike pos.symbol) option_chain with
      | Some data ->
        Printf.printf " found position symbol from option chain\n%!";
        let prev_value = pos.value in
        let value = float_of_int pos.net_qty *. data.ltp in
        Printf.printf "Updating position of symbol %s with option chain value from %f to %f \n%!" pos.symbol prev_value value;
        {
          pos with
          value;
        }
      | None ->
        Printf.printf " NO position symbol found from option chain\n%!";
        (* Option data not found — return unchanged or log warning *)
        pos
    )
    positions
