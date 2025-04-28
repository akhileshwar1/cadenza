open Lwt.Infix
open Cadenza.Order

let connect_to_data_stream (_: string) (on_message : Yojson.Safe.t -> unit) =
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
  (* Build config *)
  let config = {
    Cadenza.Strategy.data_layer_uri = "wss://yourdatastream";
    oms_layer_uri = "http://yourorderlayer";
    symbol = "NIFTY";
    local_config = ();
  } in

  let strategy = Cadenza.Alternate_strategy.create config in
  let current_strategy = ref strategy in

  let on_message (json : Yojson.Safe.t) =
    (* Printf.printf "Got JSON: %s\n%!" (Yojson.Safe.to_string json); *)
    let candle = Cadenza.Alternate_strategy.json_to_candle json in

    let old_state = (!current_strategy).state  in
    let new_state = Cadenza.Alternate_strategy.on_event old_state (Market_data_event candle) in
    current_strategy := Cadenza.Strategy.update_state !current_strategy new_state;

    (* Extract orders and print them *)
    let orders = (!current_strategy).Cadenza.Strategy.extract_orders (!current_strategy).Cadenza.Strategy.state in
    List.iter (fun order ->
      Printf.printf "New order: %s %d @ %.2f\n%!" (* %! forces printf to flush to screen instead of buffering *)
        (match order.side with | Buy -> "BUY" | Sell -> "SELL")
        order.quantity
        order.price
    ) orders

  in

  Lwt_main.run (connect_to_data_stream config.data_layer_uri on_message)
