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

let default_config = { spread_pct = 0.001; order_amount = 0.101; levels = 1 }

(* get mid price from orderbook; try get_mid_price, otherwise compute from top 1. *)
let get_mid_price_safe (ob : Orderbook.t) : float option =
    begin
      try
        let (bids, asks) = Orderbook.top_n ob ~n:1 in
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
      let buy_price = mid *. (1.0 -. (spread *. multiplier)) in
      let sell_price = mid *. (1.0 +. (spread *. multiplier)) in
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
    Printf.printf "ratios are %f %f \n%!" ratios.bid_ratio ratios.ask_ratio;
    let buys' = List.map (fun ps -> { ps with size = (Quantize.safe_quantize_size ~size:(ps.size *. ratios.bid_ratio) ~step:0.001 ~min_size:0.001 ~round:`Down) }) proposal.buys in
    let sells' = List.map (fun ps -> { ps with size = (Quantize.safe_quantize_size ~size:(ps.size *. ratios.ask_ratio) ~step:0.001 ~min_size:0.001 ~round:`Down) }) proposal.sells in
    let filtered_buys = List.filter (fun ps -> if ps.size <> 0.0 then true else false) buys' in
    let filtered_sells= List.filter (fun ps -> if ps.size <> 0.0 then true else false) sells' in
    Some { buys = filtered_buys; sells = filtered_sells}
