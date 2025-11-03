val f : ('a, unit, string) format -> 'a

val headers : (string * string) list -> string -> Cohttp.Header.t

val print_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t

val send_request :
     body:Cohttp_lwt.Body.t
  -> uri:Uri.t
  -> (string * string) list
  -> string
  -> unit Lwt.t

val project_api_preview_header : (string * string) list

val app_api_preview_header : (string * string) list

val api_json_header : (string * string) list

val github_header : Bot_info.t -> (string * string) list

val generic_get_json :
     bot_info:Bot_info.t
  -> string
  -> ?header_list:(string * string) list
  -> (Yojson.Basic.t -> 'a)
  -> ('a, string) result Lwt.t

val generic_get_zip :
     bot_info:Bot_info.t
  -> string
  -> ?header_list:(string * string) list
  -> ((Zip.entry * string) list -> 'a)
  -> ('a, string) result Lwt.t

val copy_stream :
  src:Lwt_io.input_channel -> dst:Lwt_io.output_channel -> string Lwt.t

val toml_of_string : string -> Toml.Types.table

val subkey_value : Toml.Types.table -> string -> string -> string option

val code_wrap : string -> string

val string_match : regexp:string -> ?pos:int -> string -> bool

val fold_string_matches :
  regexp:string -> f:((unit -> 'a) -> 'a) -> init:'a -> ?pos:int -> string -> 'a

val map_string_matches : regexp:string -> f:(unit -> 'a) -> string -> 'a list

val iter_string_matches : regexp:string -> f:(unit -> unit) -> string -> unit

val pr_from_branch : string -> int option * string

val first_line_of_string : string -> string

val remove_between : string -> int -> int -> string

val trim_comments : string -> string

val strip_quoted_bot_name : github_bot_name:string -> string -> string

val github_repo_of_gitlab_project_path :
     gitlab_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_domain:string
  -> gitlab_repo_full_name:string
  -> string * string

val parse_gitlab_repo_url :
  http_repo_url:string -> (string * string, string) result

val github_repo_of_gitlab_url :
     gitlab_mapping:(string, string) Base.Hashtbl.t
  -> http_repo_url:string
  -> (string * string, string) result

val download : uri:Uri.t -> string -> (unit, string) Lwt_result.t

val download_to :
  uri:Uri.t -> Lwt_io.output_channel -> (unit, string) Lwt_result.t
