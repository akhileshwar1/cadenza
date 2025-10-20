(* lib/quantize.ml *)

let pow10 n =
  let rec aux acc n =
    if n <= 0 then acc else aux (acc *. 10.) (n - 1)
  in aux 1.0 n

(* compute number of decimals from step like 0.001 -> 3 *)
let decimals_of_step step =
  if step <= 0.0 then 12
  else
    let rec count s d =
      if s *. (pow10 d) -. floor (s *. (pow10 d)) < 1e-12 then d
      else count s (d + 1)
    in
    (* limit to sane bound to avoid infinite loop *)
    let d = count step 0 in
    if d > 12 then 12 else d


(** Quantize a positive float value to the given step.
    - [size] : desired size (float)
    - [step] : size step (lot size), e.g. 0.001
    - [min_size] : minimum actionable size (defaults to step in caller)
    - [round] : `Down | `Up (must be provided)
    - ?min_notional : optional minimum notional (price * size) required by exchange (default 0.0 = disabled)
    - ?price : price used to evaluate min_notional (default 0.0)
    Returns: quantized size (float). If quantized size < min_size or quantized*price < min_notional, returns 0.0.
*)
let quantize_size
    ~size
    ~step
    ~min_size
    ~round
    ~min_notional
    ~price
  : float =
  if size <= 0.0 then 0.0
  else if step <= 0.0 then
    (* no step quantization; obey min_size and min_notional *)
    if size < min_size then 0.0
    else if min_notional > 0.0 && price > 0.0 && (size *. price) < min_notional then 0.0
    else size
  else
    let quotient = size /. step in
    let q =
      match round with
      | `Down -> floor quotient
      | `Up -> ceil quotient
    in
    let quantized = q *. step in
    (* reduce floating noise using decimals derived from step *)
    let d = decimals_of_step step in
    let factor = pow10 d in
    let quantized = (floor (quantized *. factor +. 0.5)) /. factor in
    if quantized < min_size then 0.0
    else if min_notional > 0.0 && price > 0.0 && (quantized *. price) < min_notional then
      0.0
    else
      quantized

(* safe wrapper: if original size > 0 but quantized returns 0,
   try to compute the smallest quantized size >= min_size that also meets min_notional
   and is <= original size. If none possible, return 0.0.
*)
let safe_quantize_size
    ~size
    ~step
    ~min_size
    ~round
    ~min_notional
    ~price
  : float =
  let q = quantize_size ~size ~step ~min_size ~round ~min_notional ~price in
  if q > 0.0 then q
  else
    (* quantized returned 0. Either because < min_size or < min_notional.
       Try to compute the minimal allowed quantized by constraints and see if it fits within original size. *)
    let minimal_by_size = if min_size > 0.0 then min_size else step in

    (* Compute minimal size required by min_notional, in step increments.
       If price <= 0 or min_notional <= 0 then this constraint is ignored.
    *)
    let minimal_by_notional =
      if min_notional <= 0.0 || price <= 0.0 then 0.0
      else
        let needed = min_notional /. price in
        (* round up to the nearest step *)
        let k = ceil (needed /. step) in
        k *. step
    in

    let candidate_min = max minimal_by_size minimal_by_notional in

    (* If candidate_min is 0 (no constraints) then fallback to min_size or step *)
    let candidate_min =
      if candidate_min <= 0.0 then (if min_size > 0.0 then min_size else step)
      else candidate_min
    in

    (* If candidate_min is larger than the original requested size -> can't satisfy constraints *)
    if candidate_min > size then 0.0
    else
      (* quantize candidate_min properly (round down to step grid to be safe) *)
      let cand_q = quantize_size ~size:candidate_min ~step ~min_size ~round:`Up ~min_notional ~price in
      (* quantize_size with `Up` ensures we meet or exceed candidate_min in step-grid. 
         If cand_q is still 0.0 then constraints can't be met. *)
      if cand_q = 0.0 then 0.0 else cand_q
