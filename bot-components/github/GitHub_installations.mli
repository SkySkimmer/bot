open Base

val installation_ids : (string, int) Hashtbl.t

val installation_tokens : (int, string * float) Hashtbl.t

val action_with_new_installation_token :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> install_id:int
  -> (bot_info:Bot_info.t -> 'a Lwt.t)
  -> 'a Lwt.t

val action_as_github_app_from_install_id :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> install_id:int
  -> (bot_info:Bot_info.t -> 'a Lwt.t)
  -> 'a Lwt.t

val action_as_github_app :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> owner:string
  -> (bot_info:Bot_info.t -> 'a Lwt.t)
  -> 'a Lwt.t
