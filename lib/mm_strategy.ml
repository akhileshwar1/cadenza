(* lib/mm_strategy.ml *)

(* Top-level orchestrator for market-making strategy using queue + ring producer + processor *)
let default_queue_capacity = 1024
let default_policy = Event_queue.DropOldest

type ('lc,'ls) runner = {
  mutable strategy : ('lc,'ls) Strategy.t;
  queue : Event_queue.t;
  stop_ref : bool ref;
  mutable producer_thread : unit Lwt.t option;
  mutable processor_thread : ('ls Strategy.state) Lwt.t option;
}

let create_runner ~config ~initial_local_state ~handler ~ring_path : ('lc,'ls) runner =
  let queue = Event_queue.create ~capacity:default_queue_capacity ~policy:default_policy () in
  let strat = Strategy.create config initial_local_state (fun s -> ([], s)) in
  let stop_ref = ref false in
  let runner = {
    strategy = strat;
    queue;
    stop_ref;
    producer_thread = None;
    processor_thread = None;
  } in

  (* start ring producer *)
  let prod = Ring_producer.start ~queue ~ring_path ~stop_ref in
  runner.producer_thread <- Some prod;

  (* start processor *)
  let proc_promise = Processor.start ~queue ~init_state:runner.strategy.state ~handler ~stop_ref () in
  runner.processor_thread <- Some proc_promise;

  runner

let stop runner =
  runner.stop_ref := true;
  (* processors return their final state; ensure we wait for them elsewhere if needed *)
  ()

let get_snapshot runner =
  (* return a copy of strategy; processor is responsible to keep strategy up to date *)
  runner.strategy
