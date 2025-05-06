
type option_data = {
  symbol : string;
  ltp : float;
  delta : float;
  strike : string;
}

type t = (string * (float * (string * option_data) list) list) list

val get : unit -> t Lwt.t
