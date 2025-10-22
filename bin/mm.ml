(* bin/main.ml
   Runner:
   - starts ring & oms producers that push into Event_queue
   - starts the Processor with a handler built by Processor_handler.make_handler
   - provides a simple Executor_rpc module that calls OMS REST APIs (place & cancel)
*)

open Lwt.Infix

(* Open your project top-level modules *)
open Cadenza

(* HTTP client *)
module Http = Cohttp_lwt_unix

(* Executor that calls the OMS REST endpoints.*)
module Executor_rpc : Processor_handler.EXECUTOR = Cadenza.Executor

let () =
  let ring_path =  match Sys.getenv_opt "RING_URL" with
    | Some v -> v
    | None -> "/dev/shm/aether.ring"
  in

  let oms_ws_url = match Sys.getenv_opt "OMS_WS_URL" with
    | Some v -> v
    | None -> "ws://localhost:8081/"
  in

  (* Create shared queue and stop flag *)
  let queue = Event_queue.create
    ~critical_capacity:100 (* Ensures space for OMS updates *)
    ~market_capacity:1024  (* Standard capacity for depth updates *)
    () in
  let stop_ref = ref false in

  (* Start ring producer in background *)
  Lwt.async (fun () ->
      prerr_endline ("[main] starting ring producer, ring_path=" ^ ring_path);
      Ring_producer.start ~queue ~ring_path ~stop_ref
    );

  (* Start oms producer in background *)
  Lwt.async (fun () ->
      prerr_endline ("[main] starting oms producer, ws_url=" ^ oms_ws_url);
      let login_msg = "" in
      let heartbeat_msg = "" in
      Cadenza.Oms_producer.start ~queue ~ws_url:oms_ws_url ~login_msg ~heartbeat_msg
    );

     

  (* Create local orderbook used by the handler & reconciliation *)
  let ob = Orderbook.create () in

  (* Create order tracker *)
  let tracker = Order_tracker.create () in

  (* Proposal generator and reconcile configuration defaults (tweak as needed) *)
  let pg_cfg = Proposal_generator.default_config in

  let refresh_time =
    match Sys.getenv_opt "ORDER_REFRESH_TIME" with
  | Some v -> v
  | None -> "10.0" in

  let tolerance_pct =
    match Sys.getenv_opt "TOLERANCE_PCT" with
  | Some v -> v
  | None -> "0.005" in

  let rec_cfg : Reconciler.reconcile_cfg = {
    Reconciler.refresh_tolerance_pct = float_of_string tolerance_pct;  (* 0.5% default tolerance *)
    order_refresh_time = float_of_string refresh_time;
  } in

  (* Order refresh tick producer in the background *)
  Lwt.async (fun () ->
      prerr_endline ("[main] starting refresh tick producer");
      Reconciler.reconcile_refresh_producer ~queue ~rec_cfg ~stop_ref);

   let base_bal =
    match Sys.getenv_opt "BASE_BALANCE" with
  | Some v -> v
  | None -> "97.81" in

  let quote_bal =
    match Sys.getenv_opt "QUOTE_BALANCE" with
  | Some v -> v
  | None -> "0.1711" in

  let target_base_ratio =
    match Sys.getenv_opt "TARGET_BASE_RATIO" with
  | Some v -> v
  | None -> "0.1" in

  let range_multiplier =
    match Sys.getenv_opt "RANGE_MULTIPLIER" with
  | Some v -> v
  | None -> "1.0" in

  (* Build the handler using Processor_handler.make_handler *)
  let module Exec = Executor_rpc in
  let inventory_state = Inventory_state.create 
  ~base:(float_of_string base_bal)
  ~quote:(float_of_string quote_bal)
  ~target:(float_of_string target_base_ratio)
  ~range:(float_of_string range_multiplier) () in
  let handler =
    Processor_handler.make_handler
      ~pg_cfg
      ~rec_cfg
      ~orderbook:ob
      ~tracker
      ~executor:(module Exec : Processor_handler.EXECUTOR)
      ~inventory_state
  in

  (* Install SIGINT to stop everything cleanly *)
  let _ =
    let signal_handler _ =
      prerr_endline "[main] SIGINT received, shutting down...";
      stop_ref := true
    in
    Sys.(set_signal sigint (Signal_handle signal_handler))
  in

  prerr_endline "[main] starting processor via Processor.start. Ctrl+C to exit.";
  Printexc.record_backtrace true;

  (* init_state placeholder for Strategy.state (handler ignores state currently) *)
  let init_state = Obj.magic () in

  (* Start processor (runs in background). Processor.start returns a promise which resolves when stop_ref becomes true. *)
  let processor_promise =
    Processor.start ~queue ~init_state ~handler ~stop_ref ()
  in

  (* Run until processor_promise resolves *)
  Lwt_main.run (
    processor_promise >>= fun _final_state ->
    prerr_endline "[main] processor finished, exiting.";
    Lwt.return_unit
  )
