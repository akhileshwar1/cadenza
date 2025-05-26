(* strategy.ml *)

type ('local_config, 'local_state) config = {
  data_layer_uri : string;
  oms_layer_uri : string;
  symbol : string;
  local_config : 'local_config;
  oms_ws_uri : string;
  db_conn : (module Caqti_lwt.CONNECTION) option
}

(* Define the type for the internal state *)
type 'local_state state = {
  completed_orders : Order.t list;
  pending_orders : Order.t list;
  rejected_orders : Order.t list;
  created_orders : Order.t list;
  positions : Position.t list;
  local_state : 'local_state;
}

(* Define the type for a strategy *)
type ('local_config, 'local_state) t = {
  config : ('local_config, 'local_state) config;
  state : 'local_state state;
  extract_orders : 'local_state state -> Order.t list * 'local_state state;
}

(* A function to initialize a new strategy *)
let create
  (config : ('local_config, 'local_state) config)
  (initial_local_state : 'local_state)
  (extract_orders : 'local_state state -> Order.t list * 'local_state state)
  : ('local_config, 'local_state) t =
  let initial_state = {
    completed_orders = [];
    pending_orders = [];
    rejected_orders = [];
    created_orders = [];
    positions = [];
    local_state = initial_local_state;
  } in
  {
    config;
    state = initial_state;
    extract_orders;
  }

(* A simple function to update the state *)
let update_state
  (strategy : ('local_config, 'local_state) t)
  (new_state : 'local_state state)
  : ('local_config, 'local_state) t =
  { strategy with state = new_state }
