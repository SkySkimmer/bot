type build_failure = Warn of string | Retry of string | Ignore of string

val send_status_check :
     bot_info:Bot_info.t
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> pr_num:int option
  -> string * string
  -> github_repo_full_name:string
  -> gitlab_domain:string
  -> gitlab_repo_full_name:string
  -> context:string
  -> failure_reason:string
  -> external_id:string
  -> trace:string
  -> unit Lwt.t

val trace_action : repo_full_name:string -> string -> build_failure Lwt.t

val job_failure :
     bot_info:Bot_info.t
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> pr_num:int option
  -> string * string
  -> github_repo_full_name:string
  -> gitlab_domain:string
  -> gitlab_repo_full_name:string
  -> context:string
  -> failure_reason:string
  -> external_id:string
  -> unit Lwt.t

val job_success_or_pending :
     bot_info:Bot_info.t
  -> string * string
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> github_repo_full_name:string
  -> gitlab_domain:string
  -> gitlab_repo_full_name:string
  -> context:string
  -> state:string
  -> external_id:string
  -> unit Lwt.t
