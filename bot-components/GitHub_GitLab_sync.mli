val gitlab_repo :
     bot_info:Bot_info.t
  -> gitlab_domain:string
  -> gitlab_full_name:string
  -> (string, string) Result.t
(** [gitlab_repo] constructs a GitLab repository URL with authentication token. *)

val github_repo_of_gitlab_project_path :
     gitlab_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_domain:string
  -> gitlab_repo_full_name:string
  -> string * string
(** [github_repo_of_gitlab_project_path] maps a GitLab repository to its
    corresponding GitHub repository using the provided mapping hashtable. *)

val github_repo_of_gitlab_url :
     gitlab_mapping:(string, string) Base.Hashtbl.t
  -> http_repo_url:string
  -> (string * string, string) result
(** [github_repo_of_gitlab_url] parses a GitLab repository URL and maps it to
    its corresponding GitHub repository. *)

val gitlab_ci_ref_for_github_pr :
     bot_info:Bot_info.t
  -> issue:GitHub_types.issue
  -> github_mapping:(string, string * string) Base.Hashtbl.t
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> (GitHub_types.remote_ref_info, string) Lwt_result.t
(** [gitlab_ci_ref_for_github_pr] creates a GitLab remote reference for a GitHub
    PR to enable triggering GitLab CI/CD pipelines. Returns a reference to branch
    [refs/heads/pr-<PR_NUMBER>] on the GitLab mirror of the GitHub repository. *)

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
(** [mirror_action] mirrors a GitHub branch or tag to GitLab by fetching from
    GitHub and pushing to GitLab. The local reference name is constructed from
    [base_ref] and [head_sha] to ensure uniqueness. *)
