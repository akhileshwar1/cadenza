(* src/mm_strategy.ml *)

open Strategy

(* FFI to C stub (implemented separately) *)
external caml_ring_open : string -> nativeint = "caml_ring_open"
external caml_ring_close : nativeint -> unit = "caml_ring_close"
(* returns None if no message available, or Some (msg_type, payload_string) *)
external caml_ring_read_next : nativeint -> (int * string) option = "caml_ring_read_next"


  type ring_handle = nativeint

  type ('lc,'ls) runner = {
    mutable strategy : ('lc,'ls) Strategy.t;
  mutable running : bool;
  mutable reader_thread : Thread.t option;
  mutex : Mutex.t;       (* protect strategy.state access *)
  mutable ring : ring_handle option;
  orderbook : Orderbook.t;  (* local order book maintained by this runner *)
  }

  (* FFI helpers done earlier: caml_ring_open, caml_ring_read_next, caml_ring_close *)

  (* internal on_event: takes Strategy.state and (msg_type * json) and returns updated state.
   We'll ignore Strategy-level order lists here and just update local_state.orderbook if present. *)
  let on_event_generic (runner : ('lc,'ls) runner) (_state : 'ls Strategy.state) (msg_type : int) (json : Yojson.Safe.t) : 'ls Strategy.state =
    (* We only update the runner.orderbook and print top-5. We return same Strategy.state object
      because the higher-level strategy can use state.local_state as needed. *)
    match msg_type with
  | 2 -> (* SNAPSHOT: snapshot JSON from REST with bids/asks arrays *)
      (try
        Orderbook.set_from_snapshot runner.orderbook json;
         Printf.eprintf "[mm_strategy] applied SNAPSHOT -> lastUpdateId=%Ld levels=%d\n"
           (Orderbook.last_update_id runner.orderbook) (Orderbook.total_levels runner.orderbook);
         Orderbook.print_top ~n:5 runner.orderbook;
       with ex ->
         prerr_endline ("[mm_strategy] error applying snapshot: " ^ Printexc.to_string ex));
      _state
  | 1 -> (* DEPTH_UPDATE: depth update JSON (Binance depthUpdate) *)
      (try
        let ok = Orderbook.apply_event runner.orderbook json in
        if not ok then begin
          prerr_endline "[mm_strategy] gap detected while applying depth update -> consumer should resync";
           (* In a more advanced consumer we would trigger resync via REST here *)
        end;
         Orderbook.print_top ~n:5 runner.orderbook ;
      with ex ->
        prerr_endline ("[mm_strategy] error applying depth update: " ^ Printexc.to_string ex));
      _state
  | other ->
      prerr_endline ("[mm_strategy] unknown msg_type: " ^ string_of_int other);
      _state

  (* Start reading loop: blocking/polling reader that calls internal on_event *)
  let start_reader
    ~(runner : ('lc,'ls) runner)
    ~ring_path
  =
    let ring_h = caml_ring_open ring_path in
    runner.ring <- Some ring_h;

  let do_loop () =
    try
      while runner.running do
        match caml_ring_read_next ring_h with
        | None -> Thread.delay 0.005
        | Some (msg_type, payload) ->
            (try
              let json = Yojson.Safe.from_string payload in
              Mutex.lock runner.mutex;
               let new_state = on_event_generic runner runner.strategy.state msg_type json in
               (* update strategy state (local_state unchanged here) *)
               runner.strategy <- { runner.strategy with state = new_state };
               Mutex.unlock runner.mutex;
            with
             | Yojson.Json_error e ->
                 prerr_endline ("[mm_strategy] JSON parse error in reader: " ^ e)
             | ex ->
                 prerr_endline ("[mm_strategy] reader on_event error: " ^ Printexc.to_string ex))
      done
    with ex ->
      prerr_endline ("[mm_strategy] reader loop exiting: " ^ Printexc.to_string ex)
  in
  let thr = Thread.create (fun () -> do_loop ()) () in
  runner.reader_thread <- Some thr

  (* Create and run the mm strategy; returns the runner record.
   Note: we accept an initial_local_state value and keep it in Strategy.state.local_state
         but our internal orderbook is separate in the runner. *)
  let create_and_run
    ~(config : ('lc,'ls) Strategy.config)
    ~(initial_local_state : 'ls)
    ~ring_path
  : ('lc,'ls) runner =
    let strat = Strategy.create config initial_local_state (fun s -> ([], s)) in
    let runner = {
      strategy = strat;
    running = true;
    reader_thread = None;
    mutex = Mutex.create ();
    ring = None;
    orderbook = Orderbook.create ();
               } in
    start_reader ~runner ~ring_path;
  runner

  (* Stop runner, close ring, join thread *)
  let stop (runner : ('lc,'ls) runner) =
    runner.running <- false;
  (match runner.reader_thread with
   | Some t -> (try Thread.join t with _ -> ())
   | None -> ());
  (match runner.ring with
   | Some h -> caml_ring_close h; runner.ring <- None
   | None -> ())

  (* Thread-safe accessor *)
  let get_strategy_snapshot (runner : ('lc,'ls) runner) =
    Mutex.lock runner.mutex;
  let copy = runner.strategy in
  Mutex.unlock runner.mutex;
  copy
