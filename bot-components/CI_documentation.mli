val send_doc_url :
     bot_info:Bot_info.t
  -> github_repo_full_name:string
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> unit Lwt.t
