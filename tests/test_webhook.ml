open Base
open Stdio
open Bot_components
open Cohttp

let with_captured_stderr f =
  (* Temporarily redirect stderr to a temp file so we can assert on log output. *)
  let tmp = Stdlib.Filename.temp_file "stderr" "log" in
  let tmp_fd = Unix.(openfile tmp [O_RDWR] 0o600) in
  let orig_stderr = Unix.dup Unix.stderr in
  Unix.dup2 tmp_fd Unix.stderr ;
  Unix.close tmp_fd ;
  Exn.protect
    ~f:(fun () -> f tmp)
    ~finally:(fun () ->
      Out_channel.flush Stdio.stderr ;
      Unix.dup2 orig_stderr Unix.stderr ;
      Unix.close orig_stderr )

(* Webhook payload WITH installation.id (GitHub App mode)
   The only difference from legacy webhooks is the presence of "installation" field *)
let payload =
  {|{
      "action": "opened",
      "installation": {
        "id": 12345
      },
      "repository": {
        "name": "test-repo",
        "owner": {
          "login": "test-owner"
        },
        "html_url": "https://github.com/test-owner/test-repo"
      },
      "pull_request": {
        "number": 1,
        "node_id": "PR_test123",
        "title": "Test PR",
        "html_url": "https://github.com/test-owner/test-repo/pull/1",
        "user": {
          "login": "test-user"
        },
        "labels": [],
        "body": null,
        "assignees": [],
        "base": {
          "ref": "main",
          "sha": "base123",
          "repo": {
            "html_url": "https://github.com/test-owner/test-repo"
          }
        },
        "head": {
          "ref": "feature-branch",
          "sha": "abc123",
          "repo": {
            "html_url": "https://github.com/test-owner/test-repo"
          }
        },
        "merged_at": null
      }
    }|}

(* Test webhook with installation.id *)
let test_webhook_with_installation_id () =
  let install_id = 12345 in
  let secret = "test-secret" in
  (* HMAC-SHA1: GitHub signs webhooks with HMAC(secret, payload) to
     prove authenticity and prevent tampering *)
  let signature =
    Digestif.SHA1.(to_raw_string (hmac_string ~key:secret payload))
    |> Ohex.encode
    |> fun s -> "sha1=" ^ s
  in
  let headers =
    Header.init ()
    |> fun h ->
    Header.add h "X-GitHub-Event" "pull_request"
    |> fun h -> Header.add h "X-Hub-Signature" signature
  in
  let result = GitHub_subscriptions.receive_github ~secret headers payload in
  match result with
  | Ok (Some parsed_id, _) ->
      Alcotest.(check int) "parses installation.id" install_id parsed_id
  | Ok (None, _) ->
      Alcotest.fail "Should have parsed installation.id"
  | Error msg ->
      Alcotest.fail (Printf.sprintf "Parsing failed: %s" msg)

(* Webhook payload WITHOUT installation.id (legacy webhook format)
   Identical to payload above, except missing the "installation" field.
   Note: PAT is now optional and only needed for specific flows (e.g., Rocq minimization).
   Webhooks without installation.id are still accepted by receive_github, but any action
   requiring GitHub API access will fail because action_as_github_app requires a GitHub
   App installation. *)
let payload_without_installation_id =
  {|{
    "action": "opened",
    "repository": {
      "name": "test-repo",
      "owner": {
        "login": "test-owner"
      },
      "html_url": "https://github.com/test-owner/test-repo"
    },
    "pull_request": {
      "number": 1,
      "node_id": "PR_test123",
      "title": "Test PR",
      "html_url": "https://github.com/test-owner/test-repo/pull/1",
      "user": {
        "login": "test-user"
      },
      "labels": [],
      "body": null,
      "assignees": [],
      "base": {
        "ref": "main",
        "sha": "base123",
        "repo": {
          "html_url": "https://github.com/test-owner/test-repo"
        }
      },
      "head": {
        "ref": "feature-branch",
        "sha": "abc123",
        "repo": {
          "html_url": "https://github.com/test-owner/test-repo"
        }
      },
      "merged_at": null
    }
  }|}

let test_webhook_without_installation_id () =
  let secret = "test-secret" in
  (* without installation.id, no signature required *)
  let headers =
    Header.init () |> fun h -> Header.add h "X-GitHub-Event" "pull_request"
  in
  with_captured_stderr (fun tmp_log ->
      let result =
        GitHub_subscriptions.receive_github ~secret headers
          payload_without_installation_id
      in
      ( match result with
      | Ok (None, _) ->
          ()
      | Ok (Some install_id, _) ->
          Alcotest.fail
            (Printf.sprintf "Should not have installation.id, but got: %d"
               install_id )
      | Error msg ->
          Alcotest.fail
            (Printf.sprintf
               "Webhook parsing failed unexpectedly (valid webhook without \
                installation.id should succeed): %s"
               msg ) ) ;
      Out_channel.flush stderr ;
      let logged = In_channel.read_all tmp_log in
      Alcotest.check Alcotest.bool "logs legacy webhook" true
        (String.is_substring ~substring:GitHub_subscriptions.legacy_webhook_log
           logged ) )

let () =
  Alcotest.run "Webhook tests"
    [ ( "webhook parsing"
      , [ ("with installation.id", `Quick, test_webhook_with_installation_id)
        ; ( "without installation.id (legacy webhook logs error)"
          , `Quick
          , test_webhook_without_installation_id ) ] ) ]
