(*
  inventory_skew.ml

  Port of Hummingbot's inventory skew logic (the C/Python code you showed).
  Given current base/quote balances and a target base ratio + range, compute
  bid/ask adjustment multipliers.

  The algorithm:
  - Compute total portfolio value = base_amount * price + quote_amount
  - Compute a limited base_asset_range_value = min(base_asset_range * price, total_portfolio_value * 0.5)
  - Compute left/right watermarks around target_base_asset_value
  - Interpolate base position to get inventory ratios
  - Map to bid_adjustment in [0.0 .. 2.0] and ask_adjustment = 2.0 - bid_adjustment.
*)

type ratios = {
  bid_ratio : float;
  ask_ratio : float;
}

let clamp ~lo ~hi x =
  if x < lo then lo else if x > hi then hi else x

(* linear interpolation like numpy.interp for a scalar in an interval.
   maps x in [x0, x1] -> [y0, y1]. If x outside, it is clamped to interval bounds.
*)
let interp x x0 x1 y0 y1 =
  if x1 = x0 then
    (* avoid division by zero; return midpoint of y0,y1 *)
    (y0 +. y1) /. 2.0 else let t = (x -. x0) /. (x1 -. x0) |> clamp ~lo:0.0 ~hi:1.0 in
    y0 +. t *. (y1 -. y0)

let calc_ratios
    ~(base_amount : float)
    ~(quote_amount : float)
    ~(price : float)
    ~(target_base_ratio : float)        (* between 0.0 and 1.0 *)
    ~(base_asset_range : float)         (* in base asset units *)
  : ratios =
  (* Defensive checks *)
  if price <= 0.0 || base_asset_range <= 0.0 then
    { bid_ratio = 1.0; ask_ratio = 1.0 }
  else
    let total_portfolio_value = base_amount *. price +. quote_amount in
    if total_portfolio_value <= 0.0 then
      { bid_ratio = 1.0; ask_ratio = 1.0 }
    else
      let base_asset_value = base_amount *. price in

      (* limit the base_asset_range_value to at most 50% of total portfolio value *)
      let base_asset_range_value =
        let r = base_asset_range *. price in
        if r > (0.5 *. total_portfolio_value) then 0.5 *. total_portfolio_value else r
      in

      let target_base_asset_value = total_portfolio_value *. target_base_ratio in
      let left_base_asset_value_limit = max (target_base_asset_value -. base_asset_range_value) 0.0 in
      let right_base_asset_value_limit = target_base_asset_value +. base_asset_range_value in

      (* Map base_asset_value to an inventory ratio in [0, 0.5] on the left side
         and [0.5, 1.0] on the right side (target sits at 0.5). *)
      let left_inventory_ratio =
        interp base_asset_value left_base_asset_value_limit target_base_asset_value 0.0 0.5
      in
      let right_inventory_ratio =
        interp base_asset_value target_base_asset_value right_base_asset_value_limit 0.5 1.0
      in

      (* Derive bid_adjustment:
         - if base_asset_value < target -> we are short base relative to target.
           interpolate left_inventory_ratio from [0.0..0.5] -> bid_adj [2.0..1.0]
         - else -> we are long base relative to target.
           interpolate right_inventory_ratio from [0.5..1.0] -> bid_adj [1.0..0.0]
      *)
      let bid_adjustment =
        if base_asset_value < target_base_asset_value then
          interp left_inventory_ratio 0.0 0.5 2.0 1.0
        else
          interp right_inventory_ratio 0.5 1.0 1.0 0.0
      in

      let ask_adjustment = 2.0 -. bid_adjustment in

      { bid_ratio = bid_adjustment; ask_ratio = ask_adjustment }
