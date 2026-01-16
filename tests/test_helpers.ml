open Base

let create_bot_info ?(github_install_token = "") ?(github_pat = None)
    ?(github_name = "testbot") ?(email = "test@example.com")
    ?(domain = "test.com") ?(app_id = 0) () =
  { Bot_info.github_install_token
  ; github_pat
  ; github_name
  ; email
  ; domain
  ; gitlab_instances= Hashtbl.create (module String)
  ; app_id }
