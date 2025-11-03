val f : ('a, unit, string) format -> 'a

val project_api_preview_header : (string * string) list

val app_api_preview_header : (string * string) list

val api_json_header : (string * string) list

val toml_of_string : string -> Toml.Types.table

val subkey_value : Toml.Types.table -> string -> string -> string option

val code_wrap : string -> string

val string_match : regexp:string -> ?pos:int -> string -> bool

val fold_string_matches :
  regexp:string -> f:((unit -> 'a) -> 'a) -> init:'a -> ?pos:int -> string -> 'a

val map_string_matches : regexp:string -> f:(unit -> 'a) -> string -> 'a list

val iter_string_matches : regexp:string -> f:(unit -> unit) -> string -> unit

val first_line_of_string : string -> string

val remove_between : string -> int -> int -> string

val trim_comments : string -> string

val strip_quoted_bot_name : github_bot_name:string -> string -> string
