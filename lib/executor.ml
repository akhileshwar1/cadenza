(* lib/executor.ml *)
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

(* Simple executor that calls an external OMS HTTP API to place / cancel orders.
   The OMS base URL can be provided via environment variable OMS_BASE_URL (default http://localhost:9000).
*)

let oms_base =
  match Sys.getenv_opt "OMS_BASE_URL" with
  | Some v -> v
  | None -> "http://localhost:3000"

let oms_place_path=
  match Sys.getenv_opt "OMS_PLACE_PATH" with
  | Some v -> v
  | None -> "/order/place"

let oms_cancel_path=
  match Sys.getenv_opt "OMS_CANCEL_PATH" with
  | Some v -> v
  | None -> "/order/cancel"

(* Helpers: convert order to JSON using the helper you provided in order.ml *)
let json_of_order_for_oms (order : Order.t) : Yojson.Safe.t =
  `Assoc [
    ("placed_at",
      (match order.placed_at with Some ts -> `String (Ptime.to_rfc3339 ts) | None -> `Null));
    ("executed_at",
      (match order.executed_at with Some ts -> `String (Ptime.to_rfc3339 ts) | None -> `Null));
    ("tradingsymbol", `String order.tradingsymbol);
    ("side", `String (match order.side with | Order.Buy -> "Buy" | Order.Sell -> "Sell"));
    ("validity", `String (match order.validity with | Order.DAY -> "DAY" | Order.IOC -> "IOC"));
    ("product", `String (match order.product with | Order.MIS -> "MIS" | Order.CNC -> "CNC" | Order.NRML -> "NRML"));
    ("status", `String (match order.status with
        | Some Order.Completed -> "Completed"
        | Some Order.Pending -> "Pending"
        | Some Order.Rejected -> "Rejected"
        | Some _ -> "Unknown"
        | None -> "Pending"));
    ("quantity", `Float order.quantity);
    ("filled_quantity", `Float order.filled_quantity);
    ("lot", `Int order.lot);
    ("price", `Float order.price);
    ("filled_price", `Float order.filled_price);
    ("trigger_price", `Float order.trigger_price);
    ("order_type", `String (match order.order_type with | Order.Limit -> "Limit" | Order.Market -> "Market"));
    ("exchange", `String order.exchange);
    ("strategy_name", `String order.strategy_name);
    ("order_id", `String order.order_id);
    ("broker_order_id", `String order.broker_order_id)
  ]

(* Place order by POSTing to OMS. Returns the updated order as returned by OMS (parsed into Order.t if possible). *)
let place_order ~(order : Order.t) : Order.t Lwt.t =
  let uri = Uri.of_string (oms_base ^ oms_place_path) in
  let json = json_of_order_for_oms order in
  let body = Yojson.Safe.to_string json |> Cohttp_lwt.Body.of_string in
  let headers =
    Header.init ()
    |> fun h -> Header.add h "Content-Type" "application/json"
  in
  Lwt.catch
    (fun () ->
       Client.post ~headers ~body uri >>= fun (resp, body_stream) ->
       let code = resp |> Response.status |> Cohttp.Code.code_of_status in
       Cohttp_lwt.Body.to_string body_stream >>= fun body_str ->
       if code >= 200 && code < 300 then
         (* parse response body; expect OMS returns updated order JSON *)
         (try
            let j = Yojson.Safe.from_string body_str in
            let order' = Order.of_yojson j in
            Lwt.return order'
          with _ ->
            (* If parsing fails, return original order but with broker_order_id if OMS returns something simple *)
            Lwt_io.printf "[executor] place_order: parse failed, returning original order. Resp: %s\n%!" body_str
            >>= fun () -> Lwt.return order)
       else
         Lwt.fail_with (Printf.sprintf "OMS.place_order HTTP %d: %s" code body_str)
    )
    (fun ex ->
       Lwt_io.printf "[executor] place_order error: %s %s\n%!" (oms_base ^ oms_place_path) (Printexc.to_string ex)
       >>= fun () ->
       Lwt.fail ex
    )

let cancel_order ~(order : Order.t) : Order.t Lwt.t =
  let uri = Uri.of_string (Filename.concat oms_base oms_cancel_path) in
  let json = json_of_order_for_oms order in
  let body = Yojson.Safe.to_string json |> Cohttp_lwt.Body.of_string in
  let headers = Header.init () |> fun h -> Header.add h "Content-Type" "application/json" in

  Lwt.catch
    (fun () ->
       (* DELETE with a body *)
       Client.call ~headers ~body `DELETE uri >>= fun (resp, body_stream) ->
       let code = resp |> Response.status |> Cohttp.Code.code_of_status in
       Cohttp_lwt.Body.to_string body_stream >>= fun body_str ->
       if code >= 200 && code < 300 then
         (try
            let j = Yojson.Safe.from_string body_str in
            let updated = Order.of_yojson j in
            Lwt.return updated
          with ex ->
            Lwt_io.printf "[executor] cancel_order: failed to parse response: %s\n%!" (Printexc.to_string ex)
            >>= fun () -> Lwt.return { order with status = Some Order.Cancelled })
       else
         Lwt.fail_with (Printf.sprintf "OMS.cancel_order HTTP %d: %s" code body_str)
    )
    (fun ex ->
       Lwt_io.printf "[executor] cancel_order error: %s\n%!" (Printexc.to_string ex)
       >>= fun () -> Lwt.fail ex)
