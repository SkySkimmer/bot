val f : ('a, unit, string) format -> 'a

val toml_of_string : string -> Toml.Types.table

val subkey_value : Toml.Types.table -> string -> string -> string option

val days_elapsed : float -> int

val apply_throttle : int -> ('a -> bool Lwt.t) -> 'a list -> unit Lwt.t

val format_options_for_getopts : string -> string

val getopts : string -> opt:string -> string list

val getopt : string -> opt:string -> string
