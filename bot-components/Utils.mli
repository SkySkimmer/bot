val f : ('a, unit, string) format -> 'a

val toml_of_string : string -> Toml.Types.table

val subkey_value : Toml.Types.table -> string -> string -> string option
