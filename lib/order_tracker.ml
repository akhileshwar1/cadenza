(* lib/order_tracker.ml *)
open Order

type tracked = {
  order_id : string;
  mutable broker_id : string option;
  mutable status : status_type;
  mutable qty : float;
  mutable filled_qty : float;
  created_at : Ptime.t; (* unix time *)
  order : Order.t; (* original order *)
}

type t = {
  tbl : (string, tracked) Hashtbl.t;
  mutex : Lwt_mutex.t
}

let create ?(capacity=1024) () =
  { tbl = Hashtbl.create capacity; mutex = Lwt_mutex.create () }

let register_new tracker ~order_id ~order =
  let tracked = {
    order_id;
    broker_id = None;
    status = Pending;
    qty = order.quantity;
    filled_qty = 0.0;
    created_at = Ptime_clock.now ();
    order;
  } in
  Hashtbl.replace tracker.tbl order_id tracked;
  tracked

let update_with_broker_ack tracker ~order_id ~broker_id =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    match Hashtbl.find_opt tracker.tbl order_id with
    | None -> Lwt.return_none
    | Some tr ->
      tr.broker_id <- Some broker_id;
      tr.status <- Live;
      Lwt.return_some tr
  )

let mark_filled tracker ~order_id ~filled_qty =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    match Hashtbl.find_opt tracker.tbl order_id with
    | None -> Lwt.return_none
    | Some tr ->
      tr.filled_qty <- filled_qty;
      if filled_qty >= tr.qty then tr.status <- Completed else tr.status <- Partial;
      Lwt.return_some tr
  )

let mark_cancelled tracker ~order_id =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    match Hashtbl.find_opt tracker.tbl order_id with
    | None -> Lwt.return_none
    | Some tr ->
      tr.status <- Cancelled;
      Lwt.return_some tr
  )

let mark_rejected tracker ~order_id =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    match Hashtbl.find_opt tracker.tbl order_id with
    | None -> Lwt.return_none
    | Some tr ->
      tr.status <- Rejected;
      Lwt.return_some tr
  )

let list_active tracker =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    let acc = Hashtbl.fold (fun _ v acc ->
      match v.status with
      | Live | Pending | Partial -> v :: acc
      | _ -> acc
    ) tracker.tbl [] in
    Lwt.return acc
  )

let find_by_order_id tracker order_id =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    Lwt.return (Hashtbl.find_opt tracker.tbl order_id)
  )

let find_by_broker_id tracker broker_id =
  Lwt_mutex.with_lock tracker.mutex (fun () ->
    let btrim = String.trim broker_id in
    (* debug: print lookup *)
    Printf.printf "[tracker] find_by_broker_id: looking for '%s'\n%!" btrim;

    let found =
      Hashtbl.fold (fun _key v acc ->
        match v.broker_id with
        | Some b ->
          let b_stored = String.trim b in
          (* debug each entry (comment out in hot path) *)
          Printf.printf "[tracker] - entry order_id=%s broker_id='%s'\n%!" v.order_id b_stored;
          if String.equal b_stored btrim then Some v else acc
        | None ->
          (* debug none entries lightly *)
          Printf.printf "[tracker] - entry order_id=%s broker_id=(none)\n%!" v.order_id;
          acc
      ) tracker.tbl None
    in
    Lwt.return found
  )
