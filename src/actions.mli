open Bot_components

val job_action :
     bot_info:Bot_info.t
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> unit Lwt.t

val run_ci_action :
     bot_info:Bot_info.t
  -> comment_info:GitHub_types.comment_info
  -> ?full_ci:bool
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string * string) Base.Hashtbl.t
  -> unit
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

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
