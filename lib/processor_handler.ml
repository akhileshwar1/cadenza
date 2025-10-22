(* lib/processor_handler.ml *)
open Lwt.Infix

module Event_types = Event_types
module Orderbook = Orderbook
module Proposal_generator = Proposal_generator
module Reconciler = Reconciler
module Order_tracker = Order_tracker
module Order = Order

(* Executor signature: place/cancel return Order.t Lwt.t *)
module type EXECUTOR = sig
  val place_order  : order:Order.t -> Yojson.Safe.t Lwt.t
  val cancel_order : order:Order.t -> unit Lwt.t
end

(* Helper: reconcile safely (wrap call so handler stays small) *)
let run_reconcile ~pg_cfg ~rec_cfg ~orderbook ~tracker ~executor_module ~inventory_state ~refresh_now =
  (* Reconcile.reconcile_once returns unit Lwt.t *)
  Reconciler.reconcile_once ~pg_cfg ~rec_cfg ~orderbook ~tracker ~executor:executor_module ~inventory_state ~refresh_now

let apply_fill_to_inventory ~inventory_state ~(ord : Order.t) =
  (* ord.tradingsymbol e.g. "ZECUSDT" -> base="ZEC", quote="USDT" *)
  let filled = ord.filled_quantity in
  let px = ord.filled_price in
  match ord.side with
  | Order.Buy ->
    (* You bought `filled` base at price px, so base increases, quote decreases *)
    Inventory_state.read_balances inventory_state >>= fun (base_bal, quote_bal) ->
    let base_bal' = base_bal +. filled in
    let quote_bal' = quote_bal -. (filled *. px) in
    Printf.printf "Updated balances during buy for base %f and quote %f\n%!" base_bal' quote_bal';
    Inventory_state.update_balances inventory_state ~base:base_bal' ~quote:quote_bal'
  | Order.Sell ->
    Inventory_state.read_balances inventory_state >>= fun (base_bal, quote_bal) ->
    let base_bal' = base_bal -. filled in
    let quote_bal' = quote_bal +. (filled *. px) in
    Printf.printf "Updated balances during sell for base %f and quote %f\n%!" base_bal' quote_bal';
    Inventory_state.update_balances inventory_state ~base:base_bal' ~quote:quote_bal'

let track_order_update (tracker : Order_tracker.t) (ord : Order.t) (inventory_state: Inventory_state.t) : unit Lwt.t =
  let open Order_tracker in
  let open Order in

  let filled_qty_f = ord.filled_quantity in

  (* helper to act on a found tracked record using Order_tracker helpers *)
  let handle_tracked_by_order_id order_id =
    Printf.printf "tracked order id find is %s\n%!" order_id;
    (* if the incoming update carries a broker_id, let the tracker store it *)
    (if ord.broker_order_id <> "" then
       update_with_broker_ack tracker ~order_id ~broker_id:ord.broker_order_id
       >|= ignore
     else Lwt.return_unit)
    >>= fun () ->

    (* apply fills / cancels / rejects via tracker functions *)
    match ord.status with
    | Some Completed ->
      Printf.printf "Completing %s\n%!" order_id;
      mark_filled tracker ~order_id ~filled_qty:filled_qty_f >|= ignore
      >>= fun () -> apply_fill_to_inventory ~inventory_state ~ord
    | Some Pending ->
      Printf.printf "Pending %s\n%!" order_id;
      (* treat pending as possible partial fill update *)
      if filled_qty_f > 0.0 then
        mark_filled tracker ~order_id ~filled_qty:filled_qty_f >|= ignore
        >>= fun () -> apply_fill_to_inventory ~inventory_state ~ord
    else Lwt.return_unit
    | Some Cancelled ->
      Printf.printf "Cancelling %s\n%!" order_id;
      mark_cancelled tracker ~order_id >|= ignore
    | Some Rejected ->
      Printf.printf "Rejecting %s\n%!" order_id;
      mark_rejected tracker ~order_id >|= ignore
    | _ -> Lwt.return_unit
  in

  let handle_tracked_by_broker_id broker_id =
    Printf.printf "broker id find is %s\n%!" broker_id;
    find_by_broker_id tracker broker_id >>= function
    | None -> 
        Printf.printf "broker id not found %s!!!!\n%!" broker_id;
        Lwt.return_unit
    | Some tr ->
      (* use the stored tracked.order_id to update *)
      handle_tracked_by_order_id tr.order_id
  in

  (* Main flow: try by order_id first; otherwise try by broker_id if present *)
  if ord.order_id <> "" then
    find_by_order_id tracker ord.order_id >>= function
    | Some _ -> handle_tracked_by_order_id ord.order_id
    | None ->
      if ord.broker_order_id <> "" then handle_tracked_by_broker_id ord.broker_order_id
      else Lwt.return_unit
  else
    (* no order_id in update, try broker id lookup *)
    if ord.broker_order_id <> "" then handle_tracked_by_broker_id ord.broker_order_id
    else Lwt.return_unit

(* Factory: create a handler closure capturing dependencies.
   This is still just a function returning a function (closure), so wiring is neat.
*)
let make_handler
    ~(pg_cfg : Proposal_generator.config)
    ~(rec_cfg : Reconciler.reconcile_cfg)
    ~(orderbook : Orderbook.t)
    ~(tracker : Order_tracker.t)
    ~(executor : (module EXECUTOR))
    ~(inventory_state:Inventory_state.t)
  =
  let module Ex = (val executor : EXECUTOR) in

  fun (state : 'ls Strategy.state) (ev : Event_types.event) ->
    Lwt.catch
      (fun () ->
         match ev.Event_types.typ with
         | Event_types.SNAPSHOT ->
           begin
             try
               Orderbook.set_from_snapshot orderbook ev.Event_types.payload;
               prerr_endline (Printf.sprintf "[handler] applied SNAPSHOT -> lastUpdateId=%Ld levels=%d"
                               (Orderbook.last_update_id orderbook) (Orderbook.total_levels orderbook))
             with ex ->
               prerr_endline ("[handler] error applying snapshot: " ^ Printexc.to_string ex)
           end;
           Orderbook.print_top ~n:5 orderbook;
           (* Try to generate proposal & reconcile *)
           run_reconcile ~pg_cfg ~rec_cfg ~orderbook ~tracker ~executor_module:(module Ex) ~inventory_state ~refresh_now:false
           >>= fun () ->
           Lwt.return state

         | Event_types.DEPTH_UPDATE ->
           begin
             try
               let ok = Orderbook.apply_event orderbook ev.Event_types.payload in
               if not ok then
                 prerr_endline "[handler] gap detected while applying depth update -> consumer should resync";
               prerr_endline (Printf.sprintf "[handler] depth update applied -> book.last_update_id=%Ld" (Orderbook.last_update_id orderbook))
             with ex ->
               prerr_endline ("[handler] error applying depth update: " ^ Printexc.to_string ex)
           end;
           Orderbook.print_top ~n:5 orderbook;
           run_reconcile ~pg_cfg ~rec_cfg ~orderbook ~tracker ~executor_module:(module Ex) ~inventory_state ~refresh_now:false
           >>= fun () -> Lwt.return state

         | Event_types.OMS_UPDATE ->
           (* parse OMS update into Order.t and forward to tracker and (optionally) executor tracker integration *)
           begin
             try
               (* let ev_str = Yojson.Safe.to_string ev.payload in *)
               (* Printf.printf "Ev is %s\n%!" ev_str; *)
               (* try binance-specific parser then generic fallback *)
               let ord =
                 try Order.of_yojson ev.Event_types.payload
                 with _ -> 
                   Order.of_yojson ev.Event_types.payload
               in
               (* Track update *)
               track_order_update tracker ord inventory_state >>= fun () ->
               (* optionally, if the update indicates we should take action (rare) you can call executor here *)
               Lwt.return state
             with
             | Yojson.Json_error e ->
               prerr_endline ("[handler] OMS JSON parse error: " ^ e);
               Lwt.return state
             | ex ->
               prerr_endline ("[handler] OMS handling error: " ^ Printexc.to_string ex);
               Lwt.return state
           end

         | Event_types.REFRESH ->
           prerr_endline "[handler] received REFRESH";

           run_reconcile ~pg_cfg ~rec_cfg ~orderbook ~tracker ~executor_module:(module Ex) ~inventory_state ~refresh_now:true
           >>= fun () -> Lwt.return state

         | Event_types.TICK ->
           prerr_endline "[handler] received TICK";
           Lwt.return state

         | Event_types.CUSTOM n ->
           prerr_endline (Printf.sprintf "[handler] received CUSTOM: %d" n);
           Lwt.return state)
      (fun ex ->
         prerr_endline ("[handler] exception: " ^ Printexc.to_string ex);
         Lwt.return state)
