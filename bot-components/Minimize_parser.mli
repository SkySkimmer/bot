type minimize_parsed =
  | MinimizeScript of {quote_kind: string; body: string}
  | MinimizeAttachment of {description: string; url: string}

val coqbot_minimize_text_of_body :
  github_bot_name:string -> string -> (string * minimize_parsed) option

val coqbot_ci_minimize_text_of_body :
  github_bot_name:string -> string -> (string * string list) option

val coqbot_resume_ci_minimize_text_of_body :
     github_bot_name:string
  -> string
  -> (string * string list * minimize_parsed) option

val parse_check_run_external_id : string -> (string * string) option
