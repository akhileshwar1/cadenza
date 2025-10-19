(* lib/reconciler.ml *)
open Lwt.Infix
open Event_types

(* Small helper types from your design (adapt as needed) *)
module PriceSize = struct
  type t = { price : float; size : int }
end

(* Executor module type used by reconciler *)
module type EXECUTOR_SIG = sig
  val place_order  : order:Order.t -> Yojson.Safe.t Lwt.t
  val cancel_order : order:Order.t -> unit Lwt.t
end

type reconcile_cfg = {
  refresh_tolerance_pct : float;  (* e.g. 0.01 = 1% tolerance before replacing orders *)
  order_refresh_time : float;     (* seconds between reconciliations in main loop *)
}

(* compare price floats with relative tolerance *)
let price_differs a b tol =
  if a = 0.0 then true
  else (abs_float (a -. b) /. a) > tol

(* Helper: take head safely *)
let head_opt = function
  | x::_ -> Some x
  | [] -> None

(* Reconcile once:
   - pg_cfg, orderbook, tracker are assumed to exist in your project; 
     here we only show how to call executor module.
*)
let reconcile_once
    ~(pg_cfg : Proposal_generator.config)
    ~(rec_cfg : reconcile_cfg)
    ~(orderbook : Orderbook.t)
    ~(tracker : Order_tracker.t)
    ~(executor : (module EXECUTOR_SIG))
    ~(refresh_now : bool)
  : unit Lwt.t =
  match Proposal_generator.generate ~cfg:pg_cfg ~orderbook with
  | None -> Lwt.return_unit
  | Some proposal ->
    (* get currently active tracked orders *)
    Order_tracker.list_active tracker >>= fun active ->
    let active_buys = List.filter (fun (tr:Order_tracker.tracked) -> tr.order.side = Order.Buy) active in
    let active_sells = List.filter (fun (tr:Order_tracker.tracked) -> tr.order.side = Order.Sell) active in

    (* decide creates: if no active buys, create buys; similarly for sells *)
    let to_create_buys =
      match head_opt active_buys, head_opt proposal.buys with
      | None, _ -> 
          (Printf.printf "NO ACTIVE BUYS\n%!";
          proposal.buys)
      | Some (top_active:Order_tracker.tracked), Some (top_proposal:Proposal_generator.price_size) ->
        if refresh_now then
        (Printf.printf "REFRESH PCT BUY\n%!";
          proposal.buys)
        else if price_differs top_active.order.price top_proposal.price rec_cfg.refresh_tolerance_pct then
          (Printf.printf "TOLERANCE PCT BUY\n%!";
          proposal.buys)
        else []
      | _, None -> []
    in
    let to_create_sells =
      match head_opt active_sells, head_opt proposal.sells with
      | None, _ -> 
          (Printf.printf "NO ACTIVE SELLS\n%!";
          proposal.sells)
      | Some (top_active:Order_tracker.tracked), Some (top_proposal:Proposal_generator.price_size) ->
        if refresh_now then
        (Printf.printf "REFRESH PCT SELL\n%!";
          proposal.sells)
        else if price_differs top_active.order.price top_proposal.price rec_cfg.refresh_tolerance_pct then
          (Printf.printf "TOLERANCE PCT SELL\n%!";
          proposal.sells)
        else []
      | _, None -> []
    in

    (* cancel all active if we are going to replace *)
    let cancels =
      if to_create_buys <> [] || to_create_sells <> [] then
        (Printf.printf "before execption\n%!";
        active |> List.map (fun (tr:Order_tracker.tracked) -> 
          Printf.printf "tr's order_id is %s \n%!" tr.order_id; 
          { tr.order with Order.broker_order_id = Option.get tr.broker_id })   (* map to Order.t items for executor cancel *))
      else
        []
    in

    let module Ex = (val executor : EXECUTOR_SIG) in

    (* execute cancels sequentially *)
    let rec do_cancels = function
      | [] -> Lwt.return_unit
      | (order:Order.t)::rest ->
        Ex.cancel_order ~order >>= fun _updated_order ->
        (* you might use tracker to mark cancellation; ignoring return for now *)
        Lwt.pause () >>= fun () ->
        do_cancels rest
    in

    (* create orders from PriceSize list (buys or sells) *)
    let rec do_places_for_side side = function
      | [] -> Lwt.return_unit
      | (ps:Proposal_generator.price_size)::rest ->
        let order_template =
          {
            Order.placed_at = Some (Ptime_clock.now ());
            executed_at = Some (Ptime_clock.now ());
            tradingsymbol = Orderbook.symbol_of orderbook;
            exchange = "BINANCE";
            quantity = ps.size;
            lot = 0;
            price = ps.price;
            trigger_price = 0.0;
            side = side;
            order_type = Order.Limit;
            product = Order.MIS;
            validity = Order.DAY;
            strategy_name = "";
            broker_order_id = "";
            status = Some Order.Pending;
            filled_quantity = 0.0;
            filled_price = 0.0;
            order_id = Order.generate_order_id ();
          } in
        let _ = Order_tracker.register_new tracker ~order_id:order_template.order_id ~order:order_template in
        Ex.place_order ~order:order_template >>= fun json ->
        let open Yojson.Safe.Util in
        let broker_id = json |> member "broker_order_id" |> to_string in
        Printf.printf "broker id is %s\n%!" broker_id;
        Printf.printf "order id is %s\n%!" order_template.order_id;
        Order_tracker.update_with_broker_ack tracker ~order_id:order_template.order_id ~broker_id
        >>= fun _ ->
        Lwt.pause () >>= fun () ->
        do_places_for_side side rest
    in

    do_cancels cancels >>= fun () ->
    do_places_for_side Order.Buy to_create_buys >>= fun () ->
    do_places_for_side Order.Sell to_create_sells

let rec reconcile_refresh_producer 
    ~(rec_cfg: reconcile_cfg)
    ~(queue : Event_queue.t) 
    ~stop_ref : unit Lwt.t =
  if !stop_ref then Lwt.return_unit
  else
  Lwt_unix.sleep rec_cfg.order_refresh_time >>= fun () ->
  let ev = { typ = REFRESH; payload = `Assoc []; recv_at = Ptime_clock.now () } in
  Event_queue.push queue ev >>= fun () ->
    reconcile_refresh_producer ~rec_cfg ~queue ~stop_ref
