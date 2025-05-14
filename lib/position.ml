(* lib/position.ml *)

type status =
  | Open
  | Closed

type side =
  | Buy
  | Sell

type bb = {
  candles : int;
}

type strat_pos = 
  | Bb of bb

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
  pnl: float; (* already accumulated pnl from a previous closing *)
  strat_pos : strat_pos;
}

(* this is position over all "completed" orders for a particular symbol, not meant for partial orders *)
let update_or_insert_position (positions : t list) (order : Order.t) (strat_str : string) : t list =
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
            pnl = 0.0;
            strat_pos = match strat_str with
              | "bb" -> Bb {candles = 0}
              | _ -> Bb {candles = 0};
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
            pnl = 0.0;
            strat_pos = match strat_str with
              | "bb" -> Bb {candles = 0}
              | _ -> Bb {candles = 0};
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
          let net_price, value, pnl, final_candles, status, closed_at_epoch =
            if total_qty = 0 then
              (0.0, 0.0, -.(total_buy_cost +. total_sell_cost), -1, Closed, now)
            else
              let net_price = (total_sell_cost +. total_buy_cost) /. float_of_int total_qty in
              (* keep on resetting the status to open because it may be followed by a Closed *)
              (net_price, float_of_int total_qty *. net_price, pos.pnl, 0, Open, pos.closed_at_epoch) (* t2 + 15 for the close *)
          in
          let side = if total_qty > 0 then Buy else Sell in
          Printf.printf
            "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2d\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n\
             - pnl: %.2f\n%!"
            symbol
            price
            pos.net_price
            net_price
            total_qty
            "BUY"
            pos.value
            value
            pnl;
          { pos with
            buy_qty = pos.buy_qty + qty;
            net_qty = total_qty;
            net_buy_price = new_buy_price;
            net_price = net_price;
            value = value; 
            side = side;
            opened_at_epoch = now; (* lets us handle the loading case, where close should be on t2 + 15 *)
            pnl = pnl;
            status = status;
            closed_at_epoch = closed_at_epoch;
            strat_pos = match pos.strat_pos with
                        | Bb _ -> Bb {candles = final_candles}
          }
        | Sell ->
          let total_qty = pos.net_qty - qty in
          let total_sell_cost = (float_of_int pos.sell_qty *. pos.net_sell_price) +. (-.float_of_int qty *. price) in
          let total_buy_cost = (float_of_int pos.buy_qty *. pos.net_buy_price) in
          let new_sell_price =  total_sell_cost /. float_of_int (pos.sell_qty - qty) in
          let net_price, value, pnl, final_candles, status, closed_at_epoch =
            if total_qty = 0 then
              (0.0, 0.0, -.(total_buy_cost +. total_sell_cost), -1, Closed, now)
            else
              let net_price = (total_sell_cost +. total_buy_cost) /. float_of_int total_qty in
              (net_price, float_of_int total_qty *. net_price, pos.pnl, 0, Open, pos.closed_at_epoch)
          in
          let side = if total_qty > 0 then Buy else Sell in
          Printf.printf
            "Updating position for symbol %s at price %f:\n\
             - net_price: %.2f -> %.2f\n\
             - net_qty: %.2d\n\
             - side: %s\n\
             - value: %.2f -> %.2f\n\
             - pnl: %.2f\n%!"
            symbol
            price
            pos.net_price
            net_price
            total_qty
            "SELL"
            pos.value
            value
            pnl;
          { pos with
            sell_qty = pos.sell_qty - qty;
            net_qty = total_qty;
            net_sell_price = new_sell_price;
            net_price = net_price;
            value = value;
            side = side;
            opened_at_epoch = now;
            pnl = pnl;
            status = status;
            closed_at_epoch = closed_at_epoch;
            strat_pos = match pos.strat_pos with
                        | Bb _ -> Bb {candles = final_candles}
          }
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
        let candles = match pos.strat_pos with
          | Bb b -> b.candles
        in
        Printf.printf "Updating position of symbol %s with option chain value from %f to %f and candles to %d \n%!" pos.symbol prev_value value (candles + 1);
        {
          pos with
          value;
          strat_pos = match pos.strat_pos with
                        | Bb _ -> Bb {candles = candles + 1}
        }
      | None ->
        Printf.printf " NO position symbol found from option chain\n%!";
        (* Option data not found — return unchanged or log warning *)
        pos
    )
    positions
