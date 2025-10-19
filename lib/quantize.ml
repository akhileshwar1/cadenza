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
    - [min_size] : optional minimum actionable size (defaults to step)
    - [round] : `Down | `Up (default `Down)
    Returns: quantized size (float). If quantized size < min_size, returns 0.0.
*)
let quantize_size ~size ~step ~min_size ~round : float =
  if size <= 0.0 then 0.0
  else if step <= 0.0 then if size < min_size then 0.0 else size
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
    if quantized < min_size then 0.0 else quantized

(* safe wrapper: if original size > 0 but quantized returns 0,
   return min_size instead (or step) to avoid placing 0 quantity orders. *)
let safe_quantize_size ~size ~step ~min_size ~round : float =
  let q = quantize_size ~size ~step ~min_size ~round in
  if q = 0.0 && size > 0.0 then
    (* choose the smallest placeable non-zero size *)
    let fallback = if min_size > 0.0 then min_size else step in
    (* ensure fallback is not greater than original size if you want to avoid over-sizing *)
    (* alternatively use min fallback size to be safe *)
    fallback
  else q
