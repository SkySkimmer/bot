open Base
open Bot_info
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Lwt.Infix
open Lwt.Syntax
open Zip

let f = Printf.sprintf

let headers header_list user_agent =
  Header.init ()
  |> (fun headers -> Header.add_list headers header_list)
  |> fun headers -> Header.add headers "User-Agent" user_agent

let print_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Lwt_io.printf "Response code: %d.\n" code
  >>= fun () ->
  if code < 200 && code > 299 then
    resp |> Response.headers |> Header.to_string
    |> Lwt_io.printf "Headers: %s\n"
    >>= fun () ->
    body |> Cohttp_lwt.Body.to_string >>= Lwt_io.printf "Body:\n%s\n"
  else Lwt.return_unit

let send_request ~body ~uri header_list user_agent =
  let headers = headers header_list user_agent in
  Client.post ~body ~headers uri >>= print_response

let handle_json action body =
  try
    let json = Yojson.Basic.from_string body in
    (* print_endline "JSON decoded."; *)
    Ok (action json)
  with
  | Yojson.Json_error err ->
      Error (f "Json error: %s\n" err)
  | Yojson.Basic.Util.Type_error (err, _) ->
      Error (f "Json type error: %s\n" err)

let handle_zip action body =
  let open Lwt_result.Infix in
  Lwt_io.with_temp_file (fun (tmp_name, tmp_channel) ->
      let open Lwt.Infix in
      Lwt_io.write tmp_channel body
      >>= fun () ->
      Lwt_io.close tmp_channel
      >>= Lwt_preemptive.detach (fun () ->
              try
                let zip_entries =
                  let zf = Zip.open_in tmp_name in
                  let entries =
                    Zip.entries zf
                    |> List.filter ~f:(fun entry -> not entry.is_directory)
                    |> List.map ~f:(fun entry ->
                           (entry, Zip.read_entry zf entry) )
                  in
                  Zip.close_in zf ; entries
                in
                Ok zip_entries
              with Zip.Error (zip_name, entry_name, message) ->
                Error (f "Zip.Error(%s, %s, %s)" zip_name entry_name message) ) )
  >|= action

(* GitHub specific *)

let project_api_preview_header =
  [("Accept", "application/vnd.github.inertia-preview+json")]

let app_api_preview_header =
  [("Accept", "application/vnd.github.machine-man-preview+json")]

let api_json_header = [("Accept", "application/vnd.github+json")]

let github_header bot_info =
  [("Authorization", "bearer " ^ github_token bot_info)]

let headers_of_list = headers

(* when following a redirect from GitHub to Azure, passing along the
   Authorization header results in 403 Forbidden.  So we strip the
   headers when we recurse by default. *)
let rec client_get ?(follow_redirects = true)
    ?(include_headers_in_redirects = false) ~user_agent ~headers uri =
  Client.get ~headers uri
  >>= fun (resp, body) ->
  match Response.status resp with
  | `OK ->
      Lwt.return_ok body
  | `Moved_permanently
  | `Found
  | `See_other
  | `Temporary_redirect
  | `Permanent_redirect
    when follow_redirects -> (
      let headers =
        if include_headers_in_redirects then headers
        else headers_of_list [] user_agent
      in
      match Header.get_location (Response.headers resp) with
      | Some new_uri ->
          Lwt_io.printlf "Following redirect to %s" (Uri.to_string new_uri)
          >>= fun () ->
          client_get ~follow_redirects ~include_headers_in_redirects ~headers
            ~user_agent new_uri
      | None ->
          let msg =
            f "Redirected from %s, but no Location header found"
              (Uri.to_string uri)
          in
          Lwt.return_error msg )
  | status_code ->
      let msg =
        f "HTTP request to %s failed with status code: %s" (Uri.to_string uri)
          (Code.string_of_status status_code)
      in
      Lwt.return_error msg

let generic_get ~bot_info relative_uri ?(header_list = []) handler =
  let open Lwt_result.Infix in
  let uri = "https://api.github.com/" ^ relative_uri |> Uri.of_string in
  let user_agent = bot_info.github_name in
  let headers = headers (header_list @ github_header bot_info) user_agent in
  client_get ~headers ~user_agent uri
  >>= (fun body -> Cohttp_lwt.Body.to_string body |> Lwt_result.ok)
  >>= handler

let generic_get_json ~bot_info relative_uri ?(header_list = []) json_handler =
  generic_get ~bot_info relative_uri ~header_list (fun body ->
      body |> handle_json json_handler |> Lwt.return )

let generic_get_zip ~bot_info relative_uri ?(header_list = []) zip_handler =
  generic_get ~bot_info relative_uri ~header_list (handle_zip zip_handler)

let copy_stream ~src ~dst =
  let buffer = Buffer.create 1024 in
  let rec aux () =
    Lwt_io.read_char_opt src
    >>= function
    | Some c ->
        Buffer.add_char buffer c ;
        Lwt_io.write_char dst c >>= aux
    | None ->
        Lwt.return (Buffer.contents buffer)
  in
  aux ()

let toml_of_string s = Toml.Parser.(from_string s |> unsafe)

let subkey_value toml_table k k' =
  Toml.Lenses.(get toml_table (key k |-- table |-- key k' |-- string))

let code_wrap str = f "```\n%s\n```" str

let string_match ~regexp ?(pos = 0) string =
  try
    let (_ : int) = Str.search_forward (Str.regexp regexp) string pos in
    true
  with Stdlib.Not_found -> false

let rec fold_string_matches ~regexp ~f ~init ?(pos = 0) string =
  if string_match ~regexp ~pos string then
    let pos = Str.match_end () in
    f (fun () -> fold_string_matches ~regexp ~f ~init ~pos string)
  else init

let map_string_matches ~regexp ~f string =
  fold_string_matches ~regexp
    ~f:(fun rest ->
      let v = f () in
      v :: rest () )
    ~init:[] string

let iter_string_matches ~regexp ~f string =
  fold_string_matches ~regexp ~f:(fun rest -> f () ; rest ()) ~init:() string

let pr_from_branch branch =
  if string_match ~regexp:"^pr-\\([0-9]*\\)$" branch then
    (Some (Str.matched_group 1 branch |> Int.of_string), "pull request")
  else (None, "branch")

let first_line_of_string s =
  if string_match ~regexp:"\\(.*\\)\n" s then Str.matched_group 1 s else s

let remove_between s i j =
  String.sub ~pos:0 ~len:i s ^ String.sub s ~pos:j ~len:(String.length s - j)

let trim_comments comment =
  let rec aux comment begin_ in_comment =
    if not in_comment then
      try
        let begin_ = Str.search_forward (Str.regexp "<!--") comment 0 in
        aux comment begin_ true
      with Stdlib.Not_found -> comment
    else
      try
        let end_ = Str.search_forward (Str.regexp "-->") comment begin_ in
        aux (remove_between comment begin_ (end_ + 3)) 0 false
      with Stdlib.Not_found -> comment
  in
  aux comment 0 false

let strip_quoted_bot_name ~github_bot_name body =
  (* If someone says "`@coqbot minimize foo`", (with backticks), we
     don't want to treat that as them tagging coqbot, so we adjust
     the tagging to "@`coqbot minimize foo`" so that the matching
     below doesn't pick up the name *)
  Str.global_replace
    (Str.regexp (f "\\(`\\|<code>\\)@%s:? " @@ Str.quote github_bot_name))
    (f "@\\1%s " @@ Str.quote github_bot_name)
    body

let%expect_test "strip_quoted_bot_name" =
  Stdio.printf "%s\n"
    (strip_quoted_bot_name ~github_bot_name:"coqbot"
       {|>this didn't produce a pipeline for some reason\r\n\r\nI think that this is normal. @herbelin was maybe expecting that adding the `request: full CI` label would trigger a new run immediately, but the semantics is that this label will produce such a full CI run at the next update (next push) of this PR. Cf. the [documentation](https://github.com/coq/coq/blob/master/CONTRIBUTING.md#understanding-automatic-feedback):\r\n\r\n>you can request a full run of the CI by putting the `request: full CI` label before pushing to your PR branch, or by commenting `@coqbot: run full CI` after having pushed. |} ) ;
  [%expect
    {| >this didn't produce a pipeline for some reason\r\n\r\nI think that this is normal. @herbelin was maybe expecting that adding the `request: full CI` label would trigger a new run immediately, but the semantics is that this label will produce such a full CI run at the next update (next push) of this PR. Cf. the [documentation](https://github.com/coq/coq/blob/master/CONTRIBUTING.md#understanding-automatic-feedback):\r\n\r\n>you can request a full run of the CI by putting the `request: full CI` label before pushing to your PR branch, or by commenting @`coqbot run full CI` after having pushed. |}]

let github_repo_of_gitlab_project_path ~gitlab_mapping ~gitlab_domain
    ~gitlab_repo_full_name =
  let full_name_with_domain = gitlab_domain ^ "/" ^ gitlab_repo_full_name in
  let github_full_name =
    match Hashtbl.find gitlab_mapping full_name_with_domain with
    | Some value ->
        value
    | None ->
        Stdio.printf
          "Warning: No correspondence found for GitLab repository %s.\n"
          full_name_with_domain ;
        gitlab_repo_full_name
  in
  match Str.split (Str.regexp "/") github_full_name with
  | [owner; repo] ->
      (owner, repo)
  | _ ->
      failwith
        (f "Could not split repository full name %s into (owner, repo)."
           github_full_name )

let parse_gitlab_repo_url ~http_repo_url =
  if not (string_match ~regexp:"https?://\\([^/]*\\)/\\(.*/.*\\)" http_repo_url)
  then
    Result.Error (f "Could not parse GitLab repository URL %s." http_repo_url)
  else
    Result.Ok
      (Str.matched_group 1 http_repo_url, Str.matched_group 2 http_repo_url)

let parse_gitlab_repo_url_and_print ~http_repo_url =
  match parse_gitlab_repo_url ~http_repo_url with
  | Ok (gitlab_domain, gitlab_repo_full_name) ->
      Stdio.printf "GitLab domain: \"%s\"\n" gitlab_domain ;
      Stdio.printf "GitLab repository full name: \"%s\"\n" gitlab_repo_full_name
  | Error msg ->
      Stdio.print_endline msg

let%expect_test "http_repo_url_parsing_coq" =
  parse_gitlab_repo_url_and_print ~http_repo_url:"https://gitlab.com/coq/coq" ;
  [%expect
    {|
     GitLab domain: "gitlab.com"
     GitLab repository full name: "coq/coq" |}]

let%expect_test "http_repo_url_parsing_mathcomp" =
  parse_gitlab_repo_url_and_print
    ~http_repo_url:"https://gitlab.inria.fr/math-comp/math-comp" ;
  [%expect
    {|
    GitLab domain: "gitlab.inria.fr"
    GitLab repository full name: "math-comp/math-comp" |}]

let%expect_test "http_repo_url_parsing_example_from_gitlab_docs" =
  parse_gitlab_repo_url_and_print
    ~http_repo_url:"https://gitlab.example.com/gitlab-org/gitlab-test" ;
  [%expect
    {|
    GitLab domain: "gitlab.example.com"
    GitLab repository full name: "gitlab-org/gitlab-test" |}]

let github_repo_of_gitlab_url ~gitlab_mapping ~http_repo_url =
  parse_gitlab_repo_url ~http_repo_url
  |> Result.map ~f:(fun (gitlab_domain, gitlab_repo_full_name) ->
         github_repo_of_gitlab_project_path ~gitlab_mapping ~gitlab_domain
           ~gitlab_repo_full_name )

let download_cps ~uri ~with_file =
  let open Lwt.Infix in
  let rec inner_download uri =
    let* resp, body = Client.get uri in
    match Response.status resp with
    | `OK ->
        let stream = Body.to_stream body in
        with_file (fun chan -> Lwt_stream.iter_s (Lwt_io.write chan) stream)
        >>= Lwt.return_ok
    | `Moved_permanently
    | `Found
    | `See_other
    | `Temporary_redirect
    | `Permanent_redirect -> (
      match Header.get_location (Response.headers resp) with
      | Some new_uri ->
          inner_download new_uri
      | None ->
          f "Redirected from %s, but no Location header found"
            (Uri.to_string uri)
          |> Lwt.return_error )
    | status_code ->
        f "HTTP request to %s failed with status code: %s" (Uri.to_string uri)
          (Code.string_of_status status_code)
        |> Lwt.return_error
  in
  inner_download uri

let download ~uri dest =
  download_cps ~uri ~with_file:(Lwt_io.with_file ~mode:Lwt_io.output dest)

let download_to ~uri chan =
  download_cps ~uri ~with_file:(fun write_to -> write_to chan)
