(* lib/inventory_state.ml *)

type t = {
  mutable base_balance : float;
  mutable quote_balance : float;
  mutable target_base_ratio : float;
  mutable range_multiplier : float;
  mutex : Lwt_mutex.t;
}

let create ?(base=0.0) ?(quote=0.0) ?(target=0.5) ?(range=1.0) () =
  { base_balance = base; quote_balance = quote; target_base_ratio = target; range_multiplier = range; mutex = Lwt_mutex.create () }

let update_balances t ~base ~quote =
  Lwt_mutex.with_lock t.mutex (fun () ->
    t.base_balance <- base;
    t.quote_balance <- quote;
    Lwt.return_unit)

let read_balances t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    Lwt.return (t.base_balance, t.quote_balance))

let set_target t ~target ~range_multiplier =
  Lwt_mutex.with_lock t.mutex (fun () ->
    t.target_base_ratio <- target;
    t.range_multiplier <- range_multiplier;
    Lwt.return_unit)
