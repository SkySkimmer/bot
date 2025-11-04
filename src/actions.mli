open Bot_components
open Bot_components.Minimize_parser

val job_action :
     bot_info:Bot_info.t
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> unit Lwt.t

val pipeline_action :
     bot_info:Bot_info.t
  -> GitLab_types.pipeline_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> unit Lwt.t

val run_coq_minimizer :
     bot_info:Bot_info.t
  -> script:minimize_parsed
  -> comment_thread_id:GitHub_ID.t
  -> comment_author:string
  -> owner:string
  -> repo:string
  -> options:string
  -> unit Lwt.t

val coq_bug_minimizer_results_action :
     bot_info:Bot_info.t
  -> ci:bool
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> string
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val run_bench :
     bot_info:Bot_info.t
  -> ?key_value_pairs:(string * string) list
  -> GitHub_types.comment_info
  -> unit Lwt.t

val run_ci_action :
     bot_info:Bot_info.t
  -> comment_info:GitHub_types.comment_info
  -> ?full_ci:bool
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string * string) Base.Hashtbl.t
  -> unit
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val pull_request_closed_action :
     bot_info:Bot_info.t
  -> GitHub_types.issue_info GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string * string) Base.Hashtbl.t
  -> unit Lwt.t

val pull_request_updated_action :
     bot_info:Bot_info.t
  -> action:GitHub_types.pull_request_action
  -> pr_info:GitHub_types.issue_info GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string * string) Base.Hashtbl.t
  -> (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t

val rocq_push_action :
     bot_info:Bot_info.t
  -> base_ref:string
  -> commits_msg:string list
  -> unit Lwt.t

val mirror_action :
     bot_info:Bot_info.t
  -> ?force:bool
  -> gitlab_domain:string
  -> gh_owner:string
  -> gh_repo:string
  -> gl_owner:string
  -> gl_repo:string
  -> base_ref:string
  -> head_sha:string
  -> unit
  -> unit Lwt.t

val ci_minimize :
     bot_info:Bot_info.t
  -> comment_info:GitHub_types.comment_info
  -> requests:string list
  -> comment_on_error:bool
  -> options:string
  -> bug_file:minimize_parsed option
  -> unit Lwt.t

val coq_bug_minimizer_resume_ci_minimization_action :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> string
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val rocq_check_needs_rebase_pr :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> warn_after:int
  -> close_after:int
  -> throttle:int
  -> unit Lwt.t

val rocq_check_stale_pr :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> after:int
  -> throttle:int
  -> unit Lwt.t
