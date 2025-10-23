(* lib/proposal_generator.ml *)

type price_size = {
  price : float;
  size  : float;
}

type config = {
  spread_pct : float;  (* e.g. 0.001 for 0.1% *)
  order_amount : float;  (* integer qty *)
  levels : int;        (* currently supports 1; future: multiple levels *)
}

type proposal = {
  buys : price_size list;
  sells : price_size list;
}

let spread_pct =
    match Sys.getenv_opt "SPREAD_PCT" with
  | Some v -> v
  | None -> "0.001"

let order_amount =
    match Sys.getenv_opt "ORDER_AMOUNT" with
  | Some v -> v
  | None -> "0.011"

let levels =
    match Sys.getenv_opt "LEVELS" with
  | Some v -> v
  | None -> "1"

let step =
    match Sys.getenv_opt "STEP_SIZE" with
  | Some v -> v
  | None -> "0.001"

let min_size =
    match Sys.getenv_opt "MIN_SIZE" with
  | Some v -> v
  | None -> "0.01"

let min_notional =
    match Sys.getenv_opt "MIN_NOTIONAL" with
  | Some v -> v
  | None -> "10.0"

let book_depth = 
    match Sys.getenv_opt "BOOK_DEPTH" with
    | Some v -> v
    | None -> "10"

let tick_size_precision = 
    match Sys.getenv_opt "TICK_SIZE_PRECISION" with
    | Some v -> v
    | None -> "4" (* for 0.0001 precision *)

let default_config = { spread_pct = float_of_string spread_pct; order_amount = float_of_string order_amount; levels = int_of_string levels }

(* Rounds a float to a specified number of decimal places (precision).
 * This is crucial for matching exchange tick size requirements. *)
let round_to_precision f precision =
  let p = 10.0 ** (float_of_int precision) in
  (floor (f *. p +. 0.5)) /. p

(* get mid price from orderbook; try get_mid_price, otherwise compute from top 1. *)
let get_mid_price_safe (ob : Orderbook.t) : float option =
    begin
      try
        let (bids, asks) = Orderbook.top_n ob ~n:(int_of_string book_depth) in
        match bids, asks with
        | (bp, _ ) :: _, (ap, _) :: _ -> Some ((bp +. ap) /. 2.0)
        | _ -> None
      with _ -> None
    end

(* Create a single level symmetric proposal around mid price. *)
let generate ~cfg ~orderbook : proposal option =
  match get_mid_price_safe orderbook with
  | None -> None
  | Some mid ->
    let spread = cfg.spread_pct in
    let create_level i =
      (* If future multiple levels are used: multiply spread per level *)
      let multiplier = float_of_int (i + 1) in
      let raw_buy_price = mid *. (1.0 -. (spread *. multiplier)) in
      let raw_sell_price = mid *. (1.0 +. (spread *. multiplier)) in
      let buy_price = round_to_precision raw_buy_price (int_of_string tick_size_precision) in
      let sell_price = round_to_precision raw_sell_price (int_of_string tick_size_precision) in
      ({ price = buy_price; size = cfg.order_amount },
       { price = sell_price; size = cfg.order_amount })
    in
    let rec make n acc_b acc_s =
      if n >= cfg.levels then (List.rev acc_b, List.rev acc_s)
      else
        let (b, s) = create_level n in
        make (n+1) (b::acc_b) (s::acc_s)
    in
    let (buys, sells) = make 0 [] [] in
    Some { buys; sells }


let total_size_from_proposal ~(buys : (float * float) list) ~(sells : (float * float) list) : float =
  (* assuming each item is (price, size) as floats; adjust if your PS type differs *)
  let sum_list = List.fold_left (fun acc (_p, s) -> acc +. s) 0.0 in
  let total_buys = sum_list buys in
  let total_sells = sum_list sells in
  total_buys +. total_sells

(* pseudo: wrapper around existing generate *)
let generate_with_skew ~cfg ~orderbook ~(inventory_state:Inventory_state.t) =
  match generate ~cfg ~orderbook with
  | None -> None
  | Some proposal ->
    let price = 
      match get_mid_price_safe orderbook with
      | None -> 0.0
      | Some mid -> mid
      in
    Printf.printf "MID PRICE IS %f\n%!" price;
    let total_order_size =
      total_size_from_proposal
      ~buys:(List.map (fun ps -> (ps.price, ps.size)) proposal.buys)
      ~sells:(List.map (fun ps -> (ps.price, ps.size)) proposal.sells)
    in
    let ratios = Inventory_skew.calc_ratios
      ~base_amount:inventory_state.base_balance
      ~quote_amount:inventory_state.quote_balance
      ~price
      ~target_base_ratio:inventory_state.target_base_ratio
      ~base_asset_range:(inventory_state.range_multiplier *. total_order_size)
    in
    let step = float_of_string step in
    let min_size = float_of_string min_size in
    let min_notional = float_of_string min_notional in
    let inventory_opt = Some (inventory_state.base_balance, inventory_state.quote_balance) in
    Printf.printf "ratios are %f %f \n%!" ratios.bid_ratio ratios.ask_ratio;
    let buys' = List.map (fun ps -> 
      Printf.printf "Proposal BUY %f for %f \n%!" ps.size ps.price;
      Printf.printf "Ratioed  BUY %f \n%!" (ps.size *. ratios.bid_ratio);
      { ps with size = (Quantize.safe_quantize_size ~size:(ps.size *. ratios.bid_ratio) ~step ~min_size ~round:`Down ~min_notional ~price:ps.price ~side_opt: (Some Order.Buy) ~inventory_opt) }) proposal.buys in
    let sells' = List.map (fun ps ->
      Printf.printf "Proposal SELL %f for %f \n%!" ps.size ps.price;
      Printf.printf "Ratioed  SELL %f \n%!" (ps.size *. ratios.ask_ratio);
      { ps with size = (Quantize.safe_quantize_size ~size:(ps.size *. ratios.ask_ratio) ~step ~min_size ~round:`Down ~min_notional ~price:ps.price ~side_opt: (Some Order.Sell) ~inventory_opt) }) proposal.sells in
    let filtered_buys = List.filter (fun ps -> 
      Printf.printf "Quantized BUY %f for %f \n%!" ps.size ps.price;
      if ps.size <> 0.0 then 
        (Printf.printf "buying %f for %f\n%!" ps.size ps.price;
        true) else false) buys' in
    let filtered_sells= List.filter (fun ps -> 
      Printf.printf "Quantized SELL %f for %f \n%!" ps.size ps.price;
      if ps.size <> 0.0 then (Printf.printf "selling %f for %f\n%!" ps.size ps.price;
        true) else false) sells' in
    Some { buys = filtered_buys; sells = filtered_sells}
