open Base
open Lwt.Infix

let f = Printf.sprintf

let toml_of_string s = Toml.Parser.(from_string s |> unsafe)

let subkey_value toml_table k k' =
  Toml.Lenses.(get toml_table (key k |-- table |-- key k' |-- string))

let days_elapsed ts =
  (* Yes, I know this is wrong because of DST and black holes but it should
     still be correct enough *)
  Float.to_int ((Unix.time () -. ts) /. (3600. *. 24.))

let rec apply_throttle len action args =
  if List.is_empty args || len <= 0 then Lwt.return_unit
  else
    let args, rem = List.split_n args len in
    Lwt_list.map_p action args
    >>= fun ans ->
    let n = List.count ~f:(fun b -> b) ans in
    apply_throttle (len - n) action rem
