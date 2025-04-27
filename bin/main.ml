open Lwt.Infix

let connect_to_data_stream (uri : string) (on_message : Yojson.Safe.t -> unit) =
  (* Dry-run: fake a JSON event every second *)
  let rec loop () =
    let fake_json = `Assoc [
      ("open", `Float 100.0);
      ("close", `Float 105.0);
      ("high", `Float 106.0);
      ("low", `Float 99.0);
      ("timestamp", `Float (Unix.time ()));
    ] in
    on_message fake_json;
    Lwt_unix.sleep 1.0 >>= loop
  in
  loop ()

let () =
  let open Lwt.Infix in

  (* Build config *)
  let config = {
    Cadenza.Strategy.data_layer_uri = "wss://yourdatastream";
    oms_layer_uri = "http://yourorderlayer";
    symbol = "NIFTY";
    local_config = ();
  } in

  (* Create strategy instance *)
  let strategy = Cadenza.Alternate_strategy.create config in

  (* Mutable ref to hold latest strategy *)
  let current_strategy = ref strategy in

  
  let on_message (json : Yojson.Safe.t) =
    let candle = Cadenza.Alternate_strategy.json_to_candle json in

    (* Extract full state from the current strategy *)
    let old_state = (!current_strategy).Cadenza.Strategy.state in

    (* Extract the old local state *)
    let old_local_state = old_state.local_state () in

    (* Process the event and get the new local state *)
    let new_local_state = Cadenza.Alternate_strategy.on_event old_local_state candle in

    (* Create a new state by updating only the local_state *)
    let new_state = {
      old_state with
      local_state = (fun () -> new_local_state);  (* Update the local state function *)
    } in

    (* Update the strategy with the new state *)
    current_strategy := Cadenza.Strategy.update_state !current_strategy new_state;

    (* Extract orders and print them *)
    let orders = (!current_strategy).Cadenza.Strategy.extract_orders (!current_strategy).Cadenza.Strategy.state in
    List.iter (fun order ->
      Printf.printf "New order: %s %d @ %.2f\n"
        (match order.side with | Buy -> "BUY" | Sell -> "SELL")
        order.quantity
        order.price
    ) orders

  in

  Lwt_main.run (connect_to_data_stream config.data_layer_uri on_message)
