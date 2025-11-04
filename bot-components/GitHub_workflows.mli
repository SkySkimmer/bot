val merge_pull_request_action :
  bot_info:Bot_info.t -> ?t:float -> GitHub_types.comment_info -> unit Lwt.t

val adjust_milestone :
     bot_info:Bot_info.t
  -> issue:GitHub_types.issue
  -> sleep_time:float
  -> unit Lwt.t

val project_action :
     bot_info:Bot_info.t
  -> pr_id:GitHub_ID.t
  -> backport_to:string
  -> unit
  -> unit Lwt.t

val add_to_column :
     bot_info:Bot_info.t
  -> backport_to:string
  -> [< `Card_ID of GitHub_ID.t | `PR_ID of GitHub_ID.t]
  -> string
  -> unit Lwt.t
