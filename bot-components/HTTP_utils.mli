val headers : (string * string) list -> string -> Cohttp.Header.t

val github_header : Bot_info.t -> (string * string) list

val print_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t

val send_request :
     body:Cohttp_lwt.Body.t
  -> uri:Uri.t
  -> (string * string) list
  -> string
  -> unit Lwt.t

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

val download : uri:Uri.t -> string -> (unit, string) Lwt_result.t

val download_to :
  uri:Uri.t -> Lwt_io.output_channel -> (unit, string) Lwt_result.t
