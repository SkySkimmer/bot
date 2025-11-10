(* ========================================================================== *)
(* Regex/Pattern Matching Functions *)
(* ========================================================================== *)

val string_match : regexp:string -> ?pos:int -> string -> bool

val fold_string_matches :
  regexp:string -> f:((unit -> 'a) -> 'a) -> init:'a -> ?pos:int -> string -> 'a

val map_string_matches : regexp:string -> f:(unit -> 'a) -> string -> 'a list

val iter_string_matches : regexp:string -> f:(unit -> unit) -> string -> unit

(* ========================================================================== *)
(* String Extraction and Manipulation *)
(* ========================================================================== *)

val first_line_of_string : string -> string

val remove_between : string -> int -> int -> string

(* ========================================================================== *)
(* Formatting Functions *)
(* ========================================================================== *)

val code_wrap : string -> string

(* ========================================================================== *)
(* HTML/Comment Processing *)
(* ========================================================================== *)

val trim_comments : string -> string

(* ========================================================================== *)
(* Bot-specific String Processing *)
(* ========================================================================== *)

val strip_quoted_bot_name : github_bot_name:string -> string -> string
