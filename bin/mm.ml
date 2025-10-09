(* mm.ml - entrypoint for market-maker consumer using mm_strategy *)

open Printf

let () =
  (* Build config record for Strategy.config *)
  let config : (unit, unit) Cadenza.Strategy.config = {
    Cadenza.Strategy.data_layer_uri =
      Cadenza.Connector.get_env_or_default "DATA_LAYER_URI"
        "ws://127.0.0.1:8000/candles/stream";
    oms_layer_uri =
      Cadenza.Connector.get_env_or_default "OMS_LAYER_URI"
        "http://localhost:9000/order/place";
    oms_ws_uri =
      Cadenza.Connector.get_env_or_default "OMS_WS_URI"
        "ws://localhost:8081/";
    symbol =
      Cadenza.Connector.get_env_or_default "SYMBOL" "NIFTY";
    local_config = ();
    db_conn = None;
  } in

  let initial_local_state = () in

  (* Start the mm strategy runner; returns the runner object *)
  let runner =
    Cadenza.Mm_strategy.create_and_run
      ~config
      ~initial_local_state
      ~ring_path:"/dev/shm/aether.byte.ring"
  in

  eprintf "[mm] runner started. Press Ctrl+C to stop.\n%!";
  flush stderr;

  (* Install clean shutdown on SIGINT *)
  let stop_handler _sig =
    eprintf "[mm] received SIGINT — stopping runner...\n%!";
    (try Cadenza.Mm_strategy.stop runner with ex ->
       eprintf "[mm] error while stopping runner: %s\n%!" (Printexc.to_string ex));
    eprintf "[mm] stopped. Exiting.\n%!";
    flush stderr;
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop_handler);

  (* Keep the main thread alive while background reader runs *)
  try
    while true do
      Thread.delay 1.0
    done
  with
  | Sys.Break ->
      (* fallback in case of break *)
      stop_handler Sys.sigint
