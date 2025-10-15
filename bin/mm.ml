(* bin/main.ml
   Small demo runner:
   - starts a ring reader (Ring_producer) that pushes events into an Event_queue
   - runs a processor loop that pops events, updates a local Orderbook, and prints top-5
*)

open Lwt.Infix
open Cadenza

let usage_and_exit () =
  Printf.eprintf "Usage: main <RING_PATH>\n";
  exit 2

let () =
  if Array.length Sys.argv < 2 then usage_and_exit ();
  let ring_path = Sys.argv.(1) in

  (* Create shared queue and stop flag *)
  let queue = Event_queue.create () in
  let stop_ref = ref false in

  (* Start ring producer in background *)
  Lwt.async (fun () ->
      prerr_endline ("[main] starting ring producer, ring_path=" ^ ring_path);
      Ring_producer.start ~queue ~ring_path ~stop_ref
    );

  (* Create local orderbook used by the handler *)
  let ob = Orderbook.create () in

  (* Handler invoked for each dequeued event *)
  let handle_event (ev : Event_types.event) : unit Lwt.t =
    Lwt.catch
      (fun () ->
         match ev.typ with
         | Event_types.SNAPSHOT ->
             (* Snapshot: replace entire book *)
             (try
                Orderbook.set_from_snapshot ob ev.payload;
                prerr_endline (Printf.sprintf "[main] applied SNAPSHOT -> lastUpdateId=%Ld levels=%d"
                                (Orderbook.last_update_id ob) (Orderbook.total_levels ob))
              with ex ->
                prerr_endline ("[main] error applying snapshot: " ^ Printexc.to_string ex));
             Orderbook.print_top ~n:5 ob;
             Lwt.return_unit
         | Event_types.DEPTH_UPDATE ->
             (* Depth update: apply incremental update *)
             (try
                let ok = Orderbook.apply_event ob ev.payload in
                if not ok then
                  prerr_endline "[main] gap detected while applying depth update -> consumer should resync";
                (* print whether applied and print top *)
                prerr_endline (Printf.sprintf "[main] depth update u applied -> book.last_update_id=%Ld" (Orderbook.last_update_id ob));
              with ex ->
                prerr_endline ("[main] error applying depth update: " ^ Printexc.to_string ex));
             Orderbook.print_top ~n:5 ob;
             Lwt.return_unit
         | Event_types.TICK -> 
             prerr_endline ("[main] received tick message type: ");
             Lwt.return_unit
         | Event_types.OMS_UPDATE -> 
             prerr_endline ("[main] received oms update message type: ");
             Lwt.return_unit
         | Event_types.CUSTOM n ->
             prerr_endline ("[main] received CUSTOM message type: " ^ string_of_int n);
             Lwt.return_unit)
      (fun ex ->
         prerr_endline ("[main] handler exception: " ^ Printexc.to_string ex);
         Lwt.return_unit)
  in

  (* Processor loop: pop events and call handler *)
  let rec processor_loop () =
    if !stop_ref then (
      prerr_endline "[main] stop_ref set, processor exiting";
      Lwt.return_unit
    ) else
      Event_queue.pop queue >>= fun ev ->
      handle_event ev >>= fun () ->
      processor_loop ()
  in

  (* Install SIGINT to stop everything cleanly *)
  let _ =
    let signal_handler _ =
      prerr_endline "[main] SIGINT received, shutting down...";
      stop_ref := true
    in
    Sys.(set_signal sigint (Signal_handle signal_handler))
  in

  (* Run processor loop forever (until stop_ref) *)
  prerr_endline "[main] entering processor loop. Ctrl+C to exit.";
  Lwt_main.run (processor_loop ())
