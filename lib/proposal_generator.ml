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

let default_config = { spread_pct = 0.001; order_amount = 1.0; levels = 1 }

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
