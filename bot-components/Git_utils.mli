val gitlab_repo :
     bot_info:Bot_info.t
  -> gitlab_domain:string
  -> gitlab_full_name:string
  -> (string, string) Result.t

val gitlab_ci_ref_for_github_pr :
     bot_info:Bot_info.t
  -> issue:GitHub_types.issue
  -> github_mapping:(string, string * string) Base.Hashtbl.t
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> (GitHub_types.remote_ref_info, string) Lwt_result.t
(** [gitlab_ci_ref_for_github_pr] creates a GitLab remote reference for a GitHub
  PR to enable triggering GitLab CI/CD pipelines. Returns a reference to branch
  [refs/heads/pr-<PR_NUMBER>] on the GitLab mirror of the GitHub repository.
*)

val ( |&& ) : string -> string -> string

val execute_cmd : ?mask:string list -> string -> (unit, string) result Lwt.t

val git_fetch : ?force:bool -> GitHub_types.remote_ref_info -> string -> string

val git_push :
     ?force:bool
  -> ?options:string
  -> remote_ref:GitHub_types.remote_ref_info
  -> local_ref:string
  -> unit
  -> string

val git_delete : remote_ref:GitHub_types.remote_ref_info -> string

val git_make_ancestor :
     pr_title:string
  -> pr_number:int
  -> base:string
  -> string
  -> (bool, string) result Lwt.t

val git_test_modified :
  base:string -> head:string -> string -> (bool, string) result Lwt.t

val git_coq_bug_minimizer :
     bot_info:Bot_info.t
  -> script:string
  -> comment_thread_id:GitHub_ID.t
  -> comment_author:string
  -> owner:string
  -> repo:string
  -> coq_version:string
  -> ocaml_version:string
  -> minimizer_extra_arguments:string list
  -> (unit, string) result Lwt.t

val git_run_ci_minimization :
     bot_info:Bot_info.t
  -> comment_thread_id:GitHub_ID.t
  -> owner:string
  -> repo:string
  -> pr_number:string
  -> docker_image:string
  -> target:string
  -> ci_targets:string list
  -> opam_switch:string
  -> failing_urls:string
  -> passing_urls:string
  -> base:string
  -> head:string
  -> minimizer_extra_arguments:string list
  -> bug_file_name:string option
  -> (unit, string) result Lwt.t

val pr_from_branch : string -> int option * string

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
