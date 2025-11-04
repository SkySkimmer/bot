open Base
open Bot_components
open Bot_components.Bot_info
open Bot_components.GitHub_types
open Bot_components.GitLab_types
open Cohttp
open Cohttp_lwt_unix
open Git_utils
open Utils
open Lwt.Infix
open Lwt.Syntax

let rec send_doc_url_aux ~bot_info job_info ~fallback_urls (kind, url) =
  let context = f "%s: %s artifact" job_info.build_name kind in
  let description_base = f "Link to %s build artifact" kind in
  let open Lwt.Syntax in
  let status_code url =
    let+ resp, _ = url |> Uri.of_string |> Client.get in
    resp |> Response.status |> Code.code_of_status
  in
  let success_response url =
    GitHub_mutations.send_status_check ~repo_full_name:"rocq-prover/rocq"
      ~commit:job_info.common_info.head_commit ~state:"success" ~url ~context
      ~description:(description_base ^ ".") ~bot_info
  in
  let fail_response code =
    Lwt_io.printf "But we got a %d code when checking the URL.\n" code
    <&>
    let job_url =
      f "https://gitlab.inria.fr/coq/coq/-/jobs/%d" job_info.build_id
    in
    GitHub_mutations.send_status_check ~repo_full_name:"rocq-prover/rocq"
      ~commit:job_info.common_info.head_commit ~state:"failure" ~url:job_url
      ~context
      ~description:(description_base ^ ": not found.")
      ~bot_info
  in
  let error_code url =
    let+ status_code = status_code url in
    if Int.equal 200 status_code then None else Some status_code
  in
  let* code = error_code url in
  match code with
  | None ->
      success_response url
  | Some code -> (
    match fallback_urls with
    | [] ->
        fail_response code
    | url :: fallback_urls ->
        send_doc_url_aux ~bot_info ~fallback_urls job_info (kind, url) )

let send_doc_url_job ~bot_info ?(fallback_artifacts = []) job_info doc_key
    artifact =
  Lwt_io.printf
    "This is a successful %s build. Pushing a status check with a link...\n"
    doc_key
  <&>
  let build_url artifact =
    f "https://coq.gitlabpages.inria.fr/-/coq/-/jobs/%d/artifacts/%s"
      job_info.build_id artifact
  in
  send_doc_url_aux ~bot_info job_info
    ~fallback_urls:(List.map ~f:build_url fallback_artifacts)
    (doc_key, build_url artifact)

let send_doc_url ~bot_info ~github_repo_full_name job_info =
  match (github_repo_full_name, job_info.build_name) with
  | "rocq-prover/rocq", ("doc:refman" | "doc:ci-refman") ->
      send_doc_url_job ~bot_info job_info "refman"
        "_build/default/doc/refman-html/index.html"
  | "rocq-prover/rocq", "doc:init" ->
      send_doc_url_job ~bot_info job_info "corelib"
        "_build/default/doc/corelib/html/index.html"
  | ( "rocq-prover/rocq"
    , ( "doc:stdlib" (* only after complete switch to Dune *)
      | "doc:stdlib:dune" (* only before complete switch to Dune *) ) ) ->
      send_doc_url_job ~bot_info job_info "stdlib"
        "_build/default/doc/stdlib/html/index.html"
  | "rocq-prover/rocq", "doc:ml-api:odoc" ->
      send_doc_url_job ~bot_info job_info "ml-api"
        "_build/default/_doc/_html/index.html"
  | _ ->
      Lwt.return_unit

let bench_comment ~bot_info ~owner ~repo ~number ~gitlab_url ?check_url
    (results : (Bench_utils.BenchResults.t, string) Result.t) =
  GitHub_queries.get_pull_request_id ~bot_info ~owner ~repo ~number
  >>= function
  | Ok id -> (
    match results with
    | Ok results -> (
        [ ":checkered_flag: Bench results:"
        ; String_utils.code_wrap results.summary_table
        ; results.failures
        ; String_utils.markdown_details
            (f ":turtle: Top %d slow downs" results.slow_number)
            results.slow_table
        ; String_utils.markdown_details
            (f ":rabbit2: Top %d speed ups" results.fast_number)
            results.fast_table
        ; "- "
          ^ String_utils.markdown_link ":chair: GitLab Bench Job" gitlab_url ]
        @ Option.value_map
            ~f:(fun x ->
              [ "- "
                ^ String_utils.markdown_link
                    ":spiral_notepad: Bench Check Summary" x ] )
            ~default:[] check_url
        |> String.concat ~sep:"\n"
        |> fun message ->
        GitHub_mutations.post_comment ~bot_info ~id ~message
        >>= function
        | Ok _ ->
            Lwt.return_unit
        | Error e ->
            Lwt_io.printlf "Unable to post bench comment for pr #%d: %s" number
              e )
    | Error e ->
        Lwt_io.printlf "Unable to fetch_results for bench for pr #%d: %s" number
          e )
  | Error e ->
      Lwt_io.printlf "Unable to get_pull_request_id for bench for pr #%d: %s"
        number e

let update_bench_status ~bot_info job_info (gh_owner, gh_repo) ~external_id
    ~number =
  let open Lwt.Syntax in
  match number with
  | None ->
      Lwt_io.printlf "No PR number provided for bench summary so aborting."
  | Some number -> (
      GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner ~repo:gh_repo
      >>= function
      | Error e ->
          Lwt_io.printlf "No repo id for bench job: %s" e
      | Ok repo_id -> (
          Lwt_io.printl "Pushing status check for bench job."
          <&>
          let gitlab_url =
            f "https://gitlab.inria.fr/coq/coq/-/jobs/%d" job_info.build_id
          in
          let summary =
            f "## GitLab Job URL:\n[GitLab Bench Job](%s)\n" gitlab_url
          in
          let state = job_info.build_status in
          let context = "bench" in
          let create_check_run ~status ?conclusion ~title ?(text = "") () =
            GitHub_mutations.create_check_run ~bot_info ~name:context ~status
              ~repo_id ~head_sha:job_info.common_info.head_commit ?conclusion
              ~title ~details_url:gitlab_url ~summary ~text ~external_id ()
            >>= function
            | Ok url ->
                let* () =
                  Lwt_io.printlf "Bench Check Summary updated: %s" url
                in
                Lwt.return_some url
            | Error e ->
                let* () =
                  Lwt_io.printlf "Bench Check Summary URL missing: %s" e
                in
                Lwt.return_none
          in
          match state with
          | "success" ->
              let* results = Bench_utils.fetch_bench_results ~job_info () in
              let* text = Bench_utils.bench_text results in
              let* check_url =
                create_check_run ~status:COMPLETED ~conclusion:SUCCESS
                  ~title:"Bench completed successfully" ~text ()
              in
              let* () =
                bench_comment ~bot_info ~owner:gh_owner ~repo:gh_repo ~number
                  ~gitlab_url ?check_url results
              in
              Lwt.return_unit
          | "failed" ->
              let* results = Bench_utils.fetch_bench_results ~job_info () in
              let* text = Bench_utils.bench_text results in
              let* check_url =
                create_check_run ~status:COMPLETED ~conclusion:NEUTRAL
                  ~title:"Bench completed with failures" ~text ()
              in
              let* () =
                bench_comment ~bot_info ~owner:gh_owner ~repo:gh_repo ~number
                  ~gitlab_url ?check_url results
              in
              Lwt.return_unit
          | "running" ->
              let* _ =
                create_check_run ~status:IN_PROGRESS ~title:"Bench in progress"
                  ()
              in
              Lwt.return_unit
          | "cancelled" | "canceled" ->
              let* _ =
                create_check_run ~status:COMPLETED ~conclusion:CANCELLED
                  ~title:"Bench has been cancelled" ()
              in
              Lwt.return_unit
          | "created" ->
              Lwt_io.printlf "Bench job has been created, ignoring info update."
          | _ ->
              Lwt_io.printlf "Unknown state for bench job: %s" state ) )

let job_failure ~bot_info job_info ~pr_num (gh_owner, gh_repo)
    ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name ~context
    ~failure_reason ~external_id =
  let build_id = job_info.build_id in
  let project_id = job_info.common_info.project_id in
  Lwt_io.printf "Failed job %d of project %d.\nFailure reason: %s\n" build_id
    project_id failure_reason
  >>= fun () ->
  ( if String.equal failure_reason "runner_system_failure" then
      Lwt.return (CI_utils.Retry "Runner failure reported by GitLab CI")
    else
      Lwt_io.printlf
        "Failure reason reported by GitLab CI: %s.\nRetrieving the trace..."
        failure_reason
      >>= fun () ->
      GitLab_queries.get_build_trace ~bot_info ~gitlab_domain ~project_id
        ~build_id
      >>= function
      | Ok trace ->
          CI_utils.trace_action ~repo_full_name:gitlab_repo_full_name trace
      | Error err ->
          Lwt.return
            (CI_utils.Ignore (f "Error while retrieving the trace: %s." err)) )
  >>= function
  | CI_utils.Warn trace ->
      Lwt_io.printf "Actual failure.\n"
      <&> CI_utils.send_status_check ~bot_info job_info ~pr_num
            (gh_owner, gh_repo) ~github_repo_full_name ~gitlab_domain
            ~gitlab_repo_full_name ~context ~failure_reason ~external_id ~trace
  | CI_utils.Retry reason -> (
      Lwt_io.printlf "%s... Checking whether to retry the job." reason
      >>= fun () ->
      GitLab_queries.get_retry_nb ~bot_info ~gitlab_domain
        ~full_name:gitlab_repo_full_name ~build_id
        ~build_name:job_info.build_name
      >>= function
      | Ok retry_nb when retry_nb < 3 ->
          Lwt_io.printlf
            "The job has been retried less than three times before (number of \
             retries = %d). Retrying..."
            retry_nb
          >>= fun () ->
          GitLab_mutations.retry_job ~bot_info ~gitlab_domain ~project_id
            ~build_id
      | Ok retry_nb ->
          Lwt_io.printlf
            "The job has been retried %d times before. Not retrying." retry_nb
      | Error e ->
          Lwt_io.printlf "Error while getting the number of retries: %s" e )
  | CI_utils.Ignore reason ->
      Lwt_io.printl reason

let job_success_or_pending ~bot_info (gh_owner, gh_repo)
    ({build_id} as job_info) ~github_repo_full_name ~gitlab_domain
    ~gitlab_repo_full_name ~context ~state ~external_id =
  GitHub_queries.get_status_check ~bot_info ~owner:gh_owner ~repo:gh_repo
    ~commit:job_info.common_info.head_commit ~context
  >>= function
  | Ok true -> (
      Lwt_io.printf
        "There existed a previous status check for this build, we'll override \
         it.\n"
      <&>
      let job_url =
        f "https://%s/%s/-/jobs/%d" gitlab_domain gitlab_repo_full_name build_id
      in
      let state, status, conclusion, description =
        match state with
        | "success" ->
            ( "success"
            , COMPLETED
            , Some SUCCESS
            , "Test succeeded on GitLab CI after being retried" )
        | "created" ->
            ( "pending"
            , QUEUED
            , None
            , "Test pending on GitLab CI after being retried" )
        | "running" ->
            ( "pending"
            , IN_PROGRESS
            , None
            , "Test running on GitLab CI after being retried" )
        | _ ->
            failwith
              (f "Error: job_success_or_pending received unknown state %s."
                 state )
      in
      match bot_info.github_install_token with
      | None ->
          GitHub_mutations.send_status_check ~bot_info
            ~repo_full_name:github_repo_full_name
            ~commit:job_info.common_info.head_commit ~state ~url:job_url
            ~context ~description
      | Some _ -> (
          GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
            ~repo:gh_repo
          >>= function
          | Ok repo_id ->
              let open Lwt.Syntax in
              let+ _ =
                GitHub_mutations.create_check_run ~bot_info ~name:context
                  ~status ~repo_id ~head_sha:job_info.common_info.head_commit
                  ?conclusion ~title:description ~details_url:job_url
                  ~summary:"" ~external_id ()
              in
              ()
          | Error e ->
              Lwt_io.printf "No repo id: %s\n" e ) )
  | Ok _ ->
      Lwt.return_unit
  | Error e ->
      Lwt_io.printf "%s\n" e

let job_action ~bot_info
    ({build_name; common_info= {http_repo_url}} as job_info) ~gitlab_mapping =
  let pr_num, branch_or_pr = pr_from_branch job_info.common_info.branch in
  let context = f "GitLab CI job %s (%s)" build_name branch_or_pr in
  match parse_gitlab_repo_url ~http_repo_url with
  | Error e ->
      Lwt_io.printlf "Error in job_action: %s" e
  | Ok (gitlab_domain, gitlab_repo_full_name) -> (
      let gh_owner, gh_repo =
        github_repo_of_gitlab_project_path ~gitlab_mapping ~gitlab_domain
          ~gitlab_repo_full_name
      in
      let github_repo_full_name = gh_owner ^ "/" ^ gh_repo in
      let external_id =
        f "%s,projects/%d/jobs/%d" http_repo_url job_info.common_info.project_id
          job_info.build_id
      in
      match (github_repo_full_name, job_info.build_name) with
      | "rocq-prover/rocq", "bench" ->
          update_bench_status ~bot_info job_info (gh_owner, gh_repo)
            ~external_id ~number:pr_num
      | _, _ -> (
        match job_info.build_status with
        | "failed" ->
            let failure_reason = Option.value_exn job_info.failure_reason in
            job_failure ~bot_info job_info ~pr_num (gh_owner, gh_repo)
              ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
              ~context ~failure_reason ~external_id
        | "success" as state ->
            job_success_or_pending ~bot_info (gh_owner, gh_repo) job_info
              ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
              ~context ~state ~external_id
            <&> send_doc_url ~bot_info job_info ~github_repo_full_name
        | ("created" | "running") as state ->
            job_success_or_pending ~bot_info (gh_owner, gh_repo) job_info
              ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
              ~context ~state ~external_id
        | "cancelled" | "canceled" | "pending" ->
            (* Ideally we should check if a status was already reported for
               this job.  But it is important to avoid making dozens of
               requests at once when a pipeline is canceled.  So we should
               have a caching mechanism to limit this case to a single
               request. *)
            Lwt.return_unit
        | unknown_state ->
            Lwt_io.printlf "Unknown job status: %s" unknown_state ) )

let ci_minimize ~bot_info ~comment_info ~requests ~comment_on_error ~options
    ~bug_file =
  CI_utils.minimize_failed_tests ~bot_info ~owner:comment_info.issue.issue.owner
    ~repo:comment_info.issue.issue.repo ~pr_number:comment_info.issue.number
    ~head_pipeline_summary:None
    ~request:
      ( match requests with
      | [] ->
          CI_utils.RequestSuggested
      | ["all"] ->
          CI_utils.RequestAll
      | requests ->
          CI_utils.RequestExplicit requests )
    ~comment_on_error ~options ~bug_file ()

let pipeline_action ~bot_info ({common_info= {http_repo_url}} as pipeline_info)
    ~gitlab_mapping : unit Lwt.t =
  let pr_number, _ = pr_from_branch pipeline_info.common_info.branch in
  match pipeline_info.state with
  | "skipped" ->
      Lwt.return_unit
  | _ -> (
      let pipeline_url =
        f "%s/-/pipelines/%d" http_repo_url pipeline_info.pipeline_id
      in
      let external_id =
        f "%s,projects/%d/pipelines/%d" http_repo_url
          pipeline_info.common_info.project_id pipeline_info.pipeline_id
      in
      match
        Git_utils.github_repo_of_gitlab_url ~gitlab_mapping ~http_repo_url
      with
      | Error err ->
          Lwt_io.printlf "Error in pipeline action: %s" err
      | Ok (gh_owner, gh_repo) -> (
          let state, status, conclusion, title, summary_top =
            (* For the Rocq Prover repo only, we report whether this was a full or a light CI *)
            let full_ci =
              match (gh_owner, gh_repo) with
              | "rocq-prover", "rocq" -> (
                try
                  List.find_map
                    ~f:(fun (key, value) ->
                      if String.equal key "FULL_CI" then
                        Some (Bool.of_string value)
                      else None )
                    pipeline_info.variables
                with _ -> None )
              | _ ->
                  None
            in
            let qualified_pipeline =
              match full_ci with
              | Some true ->
                  "Full pipeline"
              | Some false ->
                  "Light pipeline"
              | None ->
                  "Pipeline"
            in
            match pipeline_info.state with
            | "pending" ->
                ( "pending"
                , QUEUED
                , None
                , f "%s is pending on GitLab CI" qualified_pipeline
                , None )
            | "running" ->
                ( "pending"
                , IN_PROGRESS
                , None
                , f "%s is running on GitLab CI" qualified_pipeline
                , None )
            | "success" ->
                ( "success"
                , COMPLETED
                , Some
                    ( match full_ci with
                    | Some false ->
                        NEUTRAL
                    | Some true | None ->
                        SUCCESS )
                , f "%s completed successfully on GitLab CI" qualified_pipeline
                , None )
            | "failed" ->
                ( "failure"
                , COMPLETED
                , Some FAILURE
                , f "%s completed with errors on GitLab CI" qualified_pipeline
                , Some
                    "*If you need to restart the entire pipeline, you may do \
                     so directly in the GitHub interface using the \"Re-run\" \
                     button.*" )
            | "cancelled" | "canceled" ->
                ( "error"
                , COMPLETED
                , Some CANCELLED
                , f "%s was cancelled on GitLab CI" qualified_pipeline
                , None )
            | s ->
                ( "error"
                , COMPLETED
                , Some FAILURE
                , "Unknown pipeline status: " ^ s
                , None )
          in
          match bot_info.github_install_token with
          | None ->
              GitHub_mutations.send_status_check
                ~repo_full_name:(gh_owner ^ "/" ^ gh_repo)
                ~commit:pipeline_info.common_info.head_commit ~state
                ~url:pipeline_url
                ~context:
                  (f "GitLab CI pipeline (%s)"
                     (pr_from_branch pipeline_info.common_info.branch |> snd) )
                ~description:title ~bot_info
          | Some _ -> (
              GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
                ~repo:gh_repo
              >>= function
              | Error e ->
                  Lwt_io.printf "No repo id: %s\n" e
              | Ok repo_id -> (
                  let summary =
                    CI_utils.create_pipeline_summary ?summary_top pipeline_info
                      pipeline_url
                  in
                  GitHub_mutations.create_check_run ~bot_info
                    ~name:
                      (f "GitLab CI pipeline (%s)"
                         (pr_from_branch pipeline_info.common_info.branch |> snd) )
                    ~repo_id ~head_sha:pipeline_info.common_info.head_commit
                    ~status ?conclusion ~title ~details_url:pipeline_url
                    ~summary ~external_id ()
                  >>= fun _ ->
                  Lwt_unix.sleep 5.
                  >>= fun () ->
                  match (gh_owner, gh_repo, pipeline_info.state, pr_number) with
                  | "rocq-prover", "rocq", "failed", Some pr_number ->
                      CI_utils.minimize_failed_tests ~bot_info ~owner:gh_owner
                        ~repo:gh_repo ~pr_number
                        ~head_pipeline_summary:(Some summary)
                        ~request:CI_utils.Auto ~comment_on_error:false
                        ~options:"" ~bug_file:None
                        ?base_sha:pipeline_info.common_info.base_commit
                        ~head_sha:pipeline_info.common_info.head_commit ()
                  | _ ->
                      Lwt.return_unit ) ) ) )

let run_coq_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo ~options =
  let options = Utils.format_options_for_getopts options in
  let getopt_version opt =
    options |> Utils.getopt ~opt |> Str.replace_first (Str.regexp "^[vV]") ""
  in
  CI_utils.accumulate_extra_minimizer_arguments options
  >>= fun minimizer_extra_arguments ->
  let coq_version = getopt_version "[Cc]oq" in
  let ocaml_version = getopt_version "[Oo][Cc]aml" in
  Lwt_io.printlf
    "Parsed options for the bug minimizer at %s/%s@%s from '%s' into \
     {coq_version: '%s'; ocaml_version: '%s'; minimizer_extra_arguments: '%s'}"
    owner repo
    (GitHub_ID.to_string comment_thread_id)
    options coq_version ocaml_version
    (String.concat ~sep:" " minimizer_extra_arguments)
  >>= fun () ->
  ( match script with
  | Minimize_parser.MinimizeScript {quote_kind; body} ->
      if
        List.mem ~equal:String.equal
          ["shell"; "sh"; "shell-script"; "bash"; "zsh"]
          (String.lowercase quote_kind)
        || String.is_prefix ~prefix:"#!" body
      then
        Lwt_io.printlf "Assuming script (quote_kind: %s) is a shell script"
          quote_kind
        >>= fun () -> Lwt.return body
      else
        Lwt_io.printlf "Assuming script (quote_kind: %s) is a .v file"
          quote_kind
        >>= fun () ->
        let fname = "thebug.v" in
        Lwt.return
          (f "#!/usr/bin/env bash\ncat > %s <<'EOF'\n%s\nEOF\ncoqc -q %s" fname
             body fname )
  | Minimize_parser.MinimizeAttachment {description; url} ->
      Lwt.return
        ( "#!/usr/bin/env bash\n"
        ^ Stdlib.Filename.quote_command "./handle-web-file.sh" [description; url]
        ) )
  >>= fun script ->
  git_coq_bug_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo ~coq_version ~ocaml_version ~minimizer_extra_arguments
  >>= function
  | Ok () ->
      (* TODO: change https://github.com/rocq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f
             "Hey @%s, the coq bug minimizer [is \
              running](https://github.com/rocq-community/run-coq-bug-minimizer/actions) \
              your script, I'll come back to you with the results once it's \
              done."
             comment_author )
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment
  | Error e ->
      Lwt_io.printf "Error: %s\n" e
      >>= fun () ->
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f
             "Error encountered when attempting to start the coq bug minimizer:\n\
              %s\n\n\
              cc @JasonGross" e )
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment

let coq_bug_minimizer_results_action ~bot_info ~ci ~key ~app_id body =
  if String_utils.string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
    let stamp = Str.matched_group 1 body in
    let message = Str.matched_group 2 body in
    match Str.split (Str.regexp " ") stamp with
    | [id; author; repo_name; branch_name; owner; _repo; _ (*pr_number*)]
    | [id; author; repo_name; branch_name; owner; _repo] ->
        (fun () ->
          Github_installations.action_as_github_app ~bot_info ~key ~app_id
            ~owner
            (GitHub_mutations.post_comment ~id:(GitHub_ID.of_string id)
               ~message:(if ci then message else f "@%s, %s" author message) )
          >>= GitHub_mutations.report_on_posting_comment
          <&> ( execute_cmd
                  (* To delete the branch we need to identify as
                     coqbot the GitHub user, who is a collaborator on
                     the run-coq-bug-minimizer repo, not coqbot the
                     GitHub App *)
                  (f "git push https://%s:%s@github.com/%s.git --delete '%s'"
                     bot_info.github_name bot_info.github_pat repo_name
                     branch_name )
              >>= function
              | Ok () ->
                  Lwt.return_unit
              | Error f ->
                  Lwt_io.printf "Error: %s\n" f ) )
        |> Lwt.async ;
        Server.respond_string ~status:`OK ~body:"" ()
    | _ ->
        Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()

let coq_bug_minimizer_resume_ci_minimization_action ~bot_info ~key ~app_id body
    =
  if String_utils.string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
    let stamp = Str.matched_group 1 body in
    let message = Str.matched_group 2 body in
    match Str.split (Str.regexp " ") stamp with
    | [ comment_thread_id
      ; _author
      ; _repo_name
      ; _branch_name
      ; owner
      ; repo
      ; pr_number ] -> (
        message |> String.split ~on:'\n'
        |> function
        | docker_image :: target :: ci_targets_joined :: opam_switch
          :: failing_urls :: passing_urls :: base :: head
          :: extra_arguments_joined :: bug_file_lines ->
            (let minimizer_extra_arguments =
               String.split ~on:' ' extra_arguments_joined
             in
             let ci_targets = String.split ~on:' ' ci_targets_joined in
             let bug_file_contents = String.concat ~sep:"\n" bug_file_lines in
             fun () ->
               init_git_bare_repository ~bot_info
               >>= fun () ->
               Github_installations.action_as_github_app ~bot_info ~key ~app_id
                 ~owner
                 (CI_utils.run_ci_minimization
                    ~comment_thread_id:(GitHub_ID.of_string comment_thread_id)
                    ~owner ~repo ~base ~pr_number ~head
                    ~minimizer_extra_arguments
                    ~ci_minimization_infos:
                      [ { target
                        ; ci_targets
                        ; opam_switch
                        ; failing_urls
                        ; passing_urls
                        ; docker_image
                        ; full_target= target (* dummy value *) } ]
                    ~bug_file:
                      (Some
                         (Minimize_parser.MinimizeScript
                            {quote_kind= ""; body= bug_file_contents} ) ) )
               >>= function
               | Ok ([], []) ->
                   Lwt_io.printlf
                     "Somehow no jobs were returned from minimization \
                      resumption?\n\
                      %s"
                     message
               | Ok (jobs_minimized, jobs_that_could_not_be_minimized) -> (
                   ( match jobs_minimized with
                   | [] ->
                       Lwt.return_unit
                   | _ ->
                       Lwt_io.printlf "Resuming minimization of %s"
                         (jobs_minimized |> String.concat ~sep:", ") )
                   >>= fun () ->
                   match
                     jobs_that_could_not_be_minimized
                     |> List.map ~f:(fun (job, reason) ->
                            f "%s because %s" job reason )
                   with
                   | [] ->
                       Lwt.return_unit
                   | msgs ->
                       Lwt_io.printlf "Could not resume minimization of %s"
                         (msgs |> String.concat ~sep:", ") )
               | Error err ->
                   Lwt_io.printlf
                     "Internal error (should not happen because no url was \
                      passed):\n\
                      Could not resume minimization of %s for %s/%s#%s:\n\
                      %s"
                     target owner repo pr_number
                     (CI_utils.run_ci_minimization_error_to_string err) )
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:"Handling CI minimization resumption." ()
        | _ ->
            Server.respond_string ~status:(Code.status_of_code 400)
              ~body:
                (f
                   "Error: resume-ci-minimization called without enough \
                    arguments:\n\
                    %s"
                   message )
              () )
    | _ ->
        Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()

let rec merge_pull_request_action ~bot_info ?(t = 1.) comment_info =
  let pr = comment_info.issue in
  let reasons_for_not_merging =
    List.filter_opt
      [ ( if String.equal comment_info.author pr.user then
            Some "You are the author."
          else if
            List.exists
              ~f:(String.equal comment_info.author)
              comment_info.issue.assignees
          then None
          else Some "You are not among the assignees." )
      ; comment_info.issue.labels
        |> List.find ~f:(fun label ->
               String_utils.string_match ~regexp:"needs:.*" label )
        |> Option.map ~f:(fun l -> f "There is still a `%s` label." l)
      ; ( if
            comment_info.issue.labels
            |> List.exists ~f:(fun label ->
                   String_utils.string_match ~regexp:"kind:.*" label )
          then None
          else Some "There is no `kind:` label." )
      ; ( if comment_info.issue.milestoned then None
          else Some "No milestone has been set." ) ]
  in
  ( match reasons_for_not_merging with
  | _ :: _ ->
      let bullet_reasons =
        reasons_for_not_merging |> List.map ~f:(fun x -> "- " ^ x)
      in
      let reasons = bullet_reasons |> String.concat ~sep:"\n" in
      Lwt.return_error
        (f "@%s: You cannot merge this PR because:\n%s" comment_info.author
           reasons )
  | [] -> (
      GitHub_queries.get_pull_request_reviews_refs ~bot_info
        ~owner:pr.issue.owner ~repo:pr.issue.repo ~number:pr.issue.number
      >>= function
      | Ok reviews_info -> (
          let comment =
            List.find reviews_info.last_comments ~f:(fun c ->
                GitHub_ID.equal comment_info.id c.id )
          in
          if (not comment_info.review_comment) && Option.is_none comment then
            if Float.(t > 5.) then
              Lwt.return_error
                "Something unexpected happened: did not find merge comment \
                 after retrying three times.\n\
                 cc @rocq-prover/coqbot-maintainers"
            else
              Lwt_unix.sleep t
              >>= fun () ->
              merge_pull_request_action ~t:(t *. 2.) ~bot_info comment_info
              >>= fun () -> Lwt.return_ok ()
          else if
            (not comment_info.review_comment)
            && (Option.value_exn comment).created_by_email
            (* Option.value_exn doesn't raise an exception because comment isn't None at this point*)
          then
            Lwt.return_error
              (f
                 "@%s: Merge requests sent over e-mail are not accepted \
                  because this puts less guarantee on the authenticity of the \
                  author of the request."
                 comment_info.author )
          else if not (String.equal reviews_info.baseRef "master") then
            Lwt.return_error
              (f
                 "@%s: This PR targets branch `%s` instead of `master`. Only \
                  release managers can merge in release branches. If you are \
                  the release manager for this branch, you should use the \
                  `dev/tools/merge-pr.sh` script to merge this PR. Merging \
                  with the bot is not supported yet."
                 comment_info.author reviews_info.baseRef )
          else
            match reviews_info.review_decision with
            | NONE | REVIEW_REQUIRED ->
                Lwt.return_error
                  (f
                     "@%s: You can't merge the PR because it hasn't been \
                      approved yet."
                     comment_info.author )
            | CHANGES_REQUESTED ->
                Lwt.return_error
                  (f
                     "@%s: You can't merge the PR because some changes are \
                      requested."
                     comment_info.author )
            | APPROVED -> (
                GitHub_queries.get_team_membership ~bot_info ~org:"rocq-prover"
                  ~team:"pushers" ~user:comment_info.author
                >>= function
                | Ok false ->
                    (* User not found in the team *)
                    Lwt.return_error
                      (f
                         "@%s: You can't merge this PR because you're not a \
                          member of the `@rocq-prover/pushers` team. Look at \
                          the contributing guide for how to join this team."
                         comment_info.author )
                | Ok true -> (
                    GitHub_mutations.merge_pull_request ~bot_info ~pr_id:pr.id
                      ~commit_headline:
                        (f "Merge PR #%d: %s" pr.issue.number
                           comment_info.issue.title )
                      ~commit_body:
                        ( List.fold_left reviews_info.approved_reviews ~init:""
                            ~f:(fun s r -> s ^ f "Reviewed-by: %s\n" r )
                        ^ List.fold_left reviews_info.comment_reviews ~init:""
                            ~f:(fun s r -> s ^ f "Ack-by: %s\n" r )
                        ^ f "Co-authored-by: %s <%s@users.noreply.github.com>\n"
                            comment_info.author comment_info.author )
                      ~merge_method:MERGE ()
                    >>= fun () ->
                    match
                      List.fold_left ~init:[] reviews_info.files
                        ~f:(fun acc f ->
                          if
                            String_utils.string_match
                              ~regexp:"dev/ci/user-overlays/\\(.*\\)" f
                          then
                            let f = Str.matched_group 1 f in
                            if String.equal f "README.md" then acc else f :: acc
                          else acc )
                    with
                    | [] ->
                        Lwt.return_ok ()
                    | overlays ->
                        GitHub_mutations.post_comment ~bot_info ~id:pr.id
                          ~message:
                            (f
                               "@%s: Please take care of the following overlays:\n\
                                %s"
                               comment_info.author
                               (List.fold_left overlays ~init:"" ~f:(fun s o ->
                                    s ^ f "- %s\n" o ) ) )
                        >>= GitHub_mutations.report_on_posting_comment
                        >>= fun () -> Lwt.return_ok () )
                | Error e ->
                    Lwt.return_error
                      (f
                         "Something unexpected happened: %s\n\
                          cc @rocq-prover/coqbot-maintainers" e ) ) )
      | Error e ->
          Lwt.return_error
            (f
               "Something unexpected happened: %s\n\
                cc @rocq-prover/coqbot-maintainers" e ) ) )
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      GitHub_mutations.post_comment ~bot_info ~message:err ~id:pr.id
      >>= GitHub_mutations.report_on_posting_comment

(* TODO: ensure there's no race condition for 2 push with very close timestamps *)
let mirror_action ~bot_info ?(force = true) ~gitlab_domain ~gh_owner ~gh_repo
    ~gl_owner ~gl_repo ~base_ref ~head_sha () =
  (let open Lwt_result.Infix in
   let local_ref = base_ref ^ "-" ^ head_sha in
   let gh_ref =
     {repo_url= f "https://github.com/%s/%s" gh_owner gh_repo; name= base_ref}
   in
   (* TODO: generalize to use repository mappings, with enhanced security *)
   gitlab_repo ~bot_info ~gitlab_domain
     ~gitlab_full_name:(gl_owner ^ "/" ^ gl_repo)
   |> Lwt.return
   >>= fun gl_repo ->
   let gl_ref = {repo_url= gl_repo; name= base_ref} in
   git_fetch gh_ref local_ref |> execute_cmd
   >>= fun () -> git_push ~force ~remote_ref:gl_ref ~local_ref () |> execute_cmd
  )
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error e ->
      Lwt_io.printlf
        "Error while mirroring branch/tag %s of repository %s/%s: %s" base_ref
        gh_owner gh_repo e

(* TODO: ensure there's no race condition for 2 push with very close timestamps *)
let update_pr ?full_ci ?(skip_author_check = false) ~bot_info
    (pr_info : issue_info pull_request_info) ~gitlab_mapping ~github_mapping =
  let open Lwt_result.Infix in
  (* Try as much as possible to get unique refnames for local branches. *)
  let local_head_branch =
    f "head-%s-%s" pr_info.head.branch.name pr_info.head.sha
  in
  let local_base_branch =
    f "base-%s-%s" pr_info.base.branch.name pr_info.base.sha
  in
  git_fetch pr_info.base.branch ("refs/heads/" ^ local_base_branch)
  |&& git_fetch pr_info.head.branch ("refs/heads/" ^ local_head_branch)
  |> execute_cmd
  >>= (fun () ->
        git_make_ancestor ~pr_title:pr_info.issue.title
          ~pr_number:pr_info.issue.number ~base:local_base_branch
          local_head_branch )
  >>= fun ok ->
  let needs_full_ci_label = "needs: full CI" in
  let rebase_label = "needs: rebase" in
  let stale_label = "stale" in
  let open Lwt_result.Syntax in
  if ok then (
    (* Remove rebase / stale label *)
    GitHub_mutations.remove_labels_if_present ~bot_info pr_info.issue
      [rebase_label; stale_label] ;
    (* In the Rocq Prover repo, we want to prevent untrusted contributors from
       circumventing the fact that the bench job is a manual job by changing
       the CI configuration. *)
    let* can_trigger_ci =
      if
        String.equal pr_info.issue.issue.owner "rocq-prover"
        && String.equal pr_info.issue.issue.repo "rocq"
        && not skip_author_check
      then
        let* config_modified =
          git_test_modified ~base:pr_info.base.sha ~head:pr_info.head.sha
            ".*gitlab.*\\.yml"
        in
        if config_modified then (
          Lwt.async (fun () ->
              Lwt_io.printlf
                "CI configuration modified in PR rocq-prover/rocq#%d, checking \
                 if %s is a member of @rocq-prover/contributors..."
                pr_info.issue.number pr_info.issue.user ) ;
          (* This is an approximation:
             we are checking who the PR author is and not who is pushing. *)
          GitHub_queries.get_team_membership ~bot_info ~org:"rocq-prover"
            ~team:"contributors" ~user:pr_info.issue.user )
        else Lwt.return_ok true
      else Lwt.return_ok true
    in
    let open Lwt.Infix in
    if not can_trigger_ci then (
      (* Since we cannot trigger CI, in particular, we still need to run a full CI *)
      GitHub_mutations.add_labels_if_absent ~bot_info pr_info.issue
        [needs_full_ci_label] ;
      GitHub_mutations.post_comment ~bot_info ~id:pr_info.issue.id
        ~message:
          "I am not triggering a CI run on this PR because the CI \
           configuration has been modified. CI can be triggered manually by an \
           authorized contributor."
      >>= GitHub_mutations.report_on_posting_comment
      >>= fun () -> Lwt.return_ok () )
    else
      (* In Rocq Prover repo, we have several special cases:
         1. if something has changed in dev/ci/docker/, we rebuild the Docker image
         2. if there was a special label set, we run a full CI
      *)
      let get_options =
        if
          String.equal pr_info.issue.issue.owner "rocq-prover"
          && String.equal pr_info.issue.issue.repo "rocq"
        then
          Lwt.all
            [ ( git_test_modified ~base:pr_info.base.sha ~head:pr_info.head.sha
                  "dev/ci/docker/.*Dockerfile.*"
              >>= function
              | Ok true ->
                  Lwt.return {|-o ci.variable="SKIP_DOCKER=false"|}
              | Ok false ->
                  Lwt.return ""
              | Error e ->
                  Lwt_io.printf
                    "Error while checking if something has changed in \
                     dev/ci/docker:\n\
                     %s\n"
                    e
                  >>= fun () -> Lwt.return "" )
            ; (let request_full_ci_label = "request: full CI" in
               match full_ci with
               | Some false ->
                   (* Light CI requested *)
                   GitHub_mutations.add_labels_if_absent ~bot_info pr_info.issue
                     [needs_full_ci_label] ;
                   Lwt.return {| -o ci.variable="FULL_CI=false" |}
               | Some true ->
                   (* Full CI requested *)
                   GitHub_mutations.remove_labels_if_present ~bot_info
                     pr_info.issue
                     [needs_full_ci_label; request_full_ci_label] ;
                   Lwt.return {| -o ci.variable="FULL_CI=true" |}
               | None ->
                   (* Nothing requested with the command,
                      check if the request label is present *)
                   if
                     pr_info.issue.labels
                     |> List.exists ~f:(fun l ->
                            String.equal l request_full_ci_label )
                   then (
                     (* Full CI requested *)
                     GitHub_mutations.remove_labels_if_present ~bot_info
                       pr_info.issue
                       [needs_full_ci_label; request_full_ci_label] ;
                     Lwt.return {| -o ci.variable="FULL_CI=true" |} )
                   else (
                     (* Nothing requested *)
                     GitHub_mutations.add_labels_if_absent ~bot_info
                       pr_info.issue [needs_full_ci_label] ;
                     Lwt.return {| -o ci.variable="FULL_CI=false" |} ) ) ]
          >|= fun options -> String.concat ~sep:" " options
        else Lwt.return ""
      in
      (* Force push *)
      get_options
      >>= fun options ->
      let open Lwt_result.Infix in
      gitlab_ci_ref_for_github_pr ~issue:pr_info.issue.issue ~gitlab_mapping
        ~github_mapping ~bot_info
      >>= fun remote_ref ->
      git_push ~force:true ~options ~remote_ref ~local_ref:local_head_branch ()
      |> execute_cmd )
  else (
    (* Add rebase label if it exists *)
    GitHub_mutations.add_labels_if_absent ~bot_info pr_info.issue [rebase_label] ;
    (* Add fail status check *)
    match bot_info.github_install_token with
    | None ->
        GitHub_mutations.send_status_check
          ~repo_full_name:
            (f "%s/%s" pr_info.issue.issue.owner pr_info.issue.issue.repo)
          ~commit:pr_info.head.sha ~state:"error" ~url:""
          ~context:"GitLab CI pipeline (pull request)"
          ~description:
            "Pipeline did not run on GitLab CI because PR has conflicts with \
             base branch."
          ~bot_info
        |> Lwt_result.ok
    | Some _ -> (
        let open Lwt.Infix in
        let open Lwt.Syntax in
        GitHub_queries.get_repository_id ~bot_info
          ~owner:pr_info.issue.issue.owner ~repo:pr_info.issue.issue.repo
        >>= function
        | Ok repo_id ->
            (let+ _ =
               GitHub_mutations.create_check_run ~bot_info
                 ~name:"GitLab CI pipeline (pull request)" ~status:COMPLETED
                 ~repo_id ~head_sha:pr_info.head.sha ~conclusion:FAILURE
                 ~title:
                   "Pipeline did not run on GitLab CI because PR has conflicts \
                    with base branch."
                 ~details_url:"" ~summary:"" ()
             in
             () )
            |> Lwt_result.ok
        | Error e ->
            Lwt.return (Error e) ) )

let run_ci_action ~bot_info ~comment_info ?full_ci ~gitlab_mapping
    ~github_mapping () =
  let team = "contributors" in
  (fun () ->
    (let open Lwt_result.Infix in
     GitHub_queries.get_team_membership ~bot_info ~org:"rocq-prover" ~team
       ~user:comment_info.author
     >>= (fun is_member ->
           if is_member then
             let open Lwt.Syntax in
             let* () = Lwt_io.printl "Authorized user: pushing to GitLab." in
             match comment_info.pull_request with
             | Some pr_info ->
                 update_pr ~skip_author_check:true pr_info ~bot_info
                   ~gitlab_mapping ~github_mapping
             | None ->
                 let {owner; repo; number} = comment_info.issue.issue in
                 GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
                   ~number
                 >>= fun pr_info ->
                 update_pr ?full_ci ~skip_author_check:true
                   {pr_info with issue= comment_info.issue}
                   ~bot_info ~gitlab_mapping ~github_mapping
           else
             (* We inform the author of the request that they are not authorized. *)
             GitHub_mutations.inform_user_not_in_contributors ~bot_info
               ~comment_info
             |> Lwt_result.ok )
     |> Fn.flip Lwt_result.bind_lwt_error (fun err ->
            Lwt_io.printf "Error: %s\n" err ) )
    >>= fun _ -> Lwt.return_unit )
  |> Lwt.async ;
  Server.respond_string ~status:`OK
    ~body:
      (f
         "Received a request to run CI: checking that @%s is a member of \
          @%s/%s before doing so."
         comment_info.author comment_info.issue.issue.owner team )
    ()

let pull_request_closed_action ~bot_info
    (pr_info : GitHub_types.issue_info GitHub_types.pull_request_info)
    ~gitlab_mapping ~github_mapping =
  let open Lwt.Infix in
  gitlab_ci_ref_for_github_pr ~issue:pr_info.issue.issue ~gitlab_mapping
    ~github_mapping ~bot_info
  >>= (function
        | Ok remote_ref ->
            git_delete ~remote_ref |> execute_cmd >|= ignore
        | Error err ->
            Lwt_io.printlf "Error: %s" err )
  <&>
  if not pr_info.merged then
    Lwt_io.printf
      "PR was closed without getting merged: remove the milestone.\n"
    >>= fun () ->
    GitHub_mutations.remove_milestone pr_info.issue.issue ~bot_info
  else
    (* TODO: if PR was merged in master without a milestone, post an alert *)
    Lwt.return_unit

let pull_request_updated_action ~bot_info
    ~(action : GitHub_types.pull_request_action)
    ~(pr_info : GitHub_types.issue_info GitHub_types.pull_request_info)
    ~gitlab_mapping ~github_mapping =
  ( match (action, pr_info.base.branch.repo_url) with
  | PullRequestOpened, "https://github.com/rocq-prover/rocq"
    when String.equal pr_info.base.branch.name pr_info.head.branch.name ->
      (fun () ->
        GitHub_mutations.post_comment ~bot_info ~id:pr_info.issue.id
          ~message:
            (f
               "Hello, thanks for your pull request!\n\
                In the future, we strongly recommend that you *do not* use %s \
                as the name of your branch when submitting a pull request.\n\
                By the way, you may be interested in reading [our contributing \
                guide](https://github.com/rocq-prover/rocq/blob/master/CONTRIBUTING.md)."
               pr_info.base.branch.name )
        >>= GitHub_mutations.report_on_posting_comment )
      |> Lwt.async
  | _ ->
      () ) ;
  (fun () ->
    update_pr pr_info ~bot_info ~gitlab_mapping ~github_mapping
    >>= fun _ -> Lwt.return_unit )
  |> Lwt.async ;
  Server.respond_string ~status:`OK
    ~body:
      (f
         "Pull request %s/%s#%d was (re)opened / synchronized: (force-)pushing \
          to GitLab."
         pr_info.issue.issue.owner pr_info.issue.issue.repo
         pr_info.issue.issue.number )
    ()

let rec adjust_milestone ~bot_info ~issue ~sleep_time =
  (* We implement an exponential backoff strategy to try again after
     5, 25, and 125 seconds, if the issue was closed by a commit not
     yet associated to a pull request or if we couldn't find the close
     event. *)
  GitHub_queries.get_issue_closer_info ~bot_info issue
  >>= function
  | Ok (ClosedByPullRequest result) ->
      GitHub_mutations.reflect_pull_request_milestone ~bot_info result
  | Ok ClosedByCommit when Float.(sleep_time > 200.) ->
      Lwt_io.print "Closed by commit not associated to any pull request.\n"
  | Ok NoCloseEvent when Float.(sleep_time > 200.) ->
      Lwt_io.printf "Error: no close event after 200 seconds.\n"
  | Ok (ClosedByCommit | NoCloseEvent) ->
      (* May be worth trying again later. *)
      Lwt_io.printf
        "Closed by commit not yet associated to any pull request or no close \
         event yet...\n\
        \ Trying again in %f seconds.\n"
        sleep_time
      >>= (fun () -> Lwt_unix.sleep sleep_time)
      >>= fun () ->
      adjust_milestone ~issue ~sleep_time:(sleep_time *. 5.) ~bot_info
  | Ok ClosedByOther ->
      (* Not worth trying again *)
      Lwt_io.print "Not closed by pull request or commit.\n"
  | Error err ->
      Lwt_io.print (f "Error: %s\n" err)

let project_action ~bot_info ~pr_id ~backport_to () =
  GitHub_queries.get_pull_request_milestone ~bot_info ~pr_id
  >>= function
  | Error err ->
      Lwt_io.printf "Error: %s\n" err
  | Ok backport_info -> (
    match
      List.find_map backport_info
        ~f:(fun {backport_to= backport_to'; rejected_milestone} ->
          if String.equal backport_to backport_to' then Some rejected_milestone
          else None )
    with
    | None ->
        Lwt_io.printf
          "PR already not in milestone with backporting info for branch %s.\n"
          backport_to
    | Some rejected_milestone -> (
        Lwt_io.printf
          "PR is in milestone for which backporting to %s was rejected.\n\
           Change of milestone requested.\n"
          backport_to
        >>= fun () ->
        GitHub_queries.get_milestone_id ~bot_info ~owner:"rocq-prover"
          ~repo:"rocq" ~number:rejected_milestone
        >>= function
        | Ok milestone ->
            GitHub_mutations.update_milestone_pull_request ~bot_info ~pr_id
              ~milestone
            <&> ( GitHub_mutations.post_comment ~bot_info ~id:pr_id
                    ~message:
                      "This PR was postponed. Please update accordingly the \
                       milestone of any issue that this fixes as this cannot \
                       be done automatically."
                >>= GitHub_mutations.report_on_posting_comment )
        | Error err ->
            Lwt_io.printlf "Error while obtaining milestone ID: %s" err ) )

let add_to_column ~bot_info ~backport_to id option =
  let field = backport_to ^ " status" in
  GitHub_queries.get_project_field_values ~bot_info ~organization:"rocq-prover"
    ~project:11 ~field ~options:[|option|]
  >>= fun project_info ->
  ( match project_info with
  | Ok (project_id, Some (field_id, [(option', field_value_id)]))
    when String.equal option option' ->
      Lwt.return_ok (project_id, field_id, field_value_id)
  | Ok (_, Some (_, [])) ->
      Lwt.return_error
        (f "Error: Could not find '%s' option in the field." option)
  | Ok (_, Some _) ->
      Lwt.return_error
        (f "Error: Unexpected result when looking for '%s'." option)
  | Ok (project_id, None) -> (
      Lwt_io.printlf
        "Required backporting field '%s' does not exist yet. Creating it..."
        field
      >>= fun () ->
      GitHub_mutations.create_new_release_management_field ~bot_info ~project_id
        ~field
      >>= function
      | Ok (field_id, options) -> (
        match
          List.find_map options ~f:(fun (option', field_value_id) ->
              if String.equal option option' then Some field_value_id else None )
        with
        | Some field_value_id ->
            Lwt.return_ok (project_id, field_id, field_value_id)
        | None ->
            Lwt.return_error
              (f
                 "Error new field '%s status' was created, but does not have a \
                  '%s' option."
                 field option ) )
      | Error err ->
          Lwt.return_error
            (f "Error while creating new backporting field '%s': %s" field err)
      )
  | Error err ->
      Lwt.return_error (f "Error while getting project field values: %s" err) )
  >>= function
  | Ok (project_id, field_id, field_value_id) -> (
      ( match id with
      | `PR_ID card_id ->
          GitHub_mutations.add_card_to_project ~bot_info ~card_id ~project_id
      | `Card_ID card_id ->
          Lwt.return_ok card_id )
      >>= fun result ->
      match result with
      | Ok card_id ->
          GitHub_mutations.update_field_value ~bot_info ~card_id ~project_id
            ~field_id ~field_value_id
      | Error err ->
          Lwt_io.printf "Error while adding card to project: %s\n" err )
  | Error err ->
      Lwt_io.printl err

let rocq_push_action ~bot_info ~base_ref ~commits_msg =
  let* () = Lwt_io.printl "Merge and backport commit messages:" in
  let commit_action commit_msg =
    if
      String_utils.string_match
        ~regexp:"^Merge \\(PR\\|pull request\\) #\\([0-9]*\\)" commit_msg
    then
      let pr_number = Str.matched_group 2 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was merged.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.get_pull_request_id_and_milestone ~bot_info
        ~owner:"rocq-prover" ~repo:"rocq" ~number:pr_number
      >>= fun pr_info ->
      match pr_info with
      | Ok (pr_id, backport_info) ->
          backport_info
          |> Lwt_list.iter_p (fun {backport_to} ->
                 if "refs/heads/" ^ backport_to |> String.equal base_ref then
                   Lwt_io.printf
                     "PR was merged into the backporting branch directly.\n"
                   >>= fun () ->
                   add_to_column ~bot_info ~backport_to (`PR_ID pr_id) "Shipped"
                 else if String.equal base_ref "refs/heads/master" then
                   (* For now, we hard code that PRs are only backported
                      from master.  In the future, we could make this
                      configurable in the milestone description or in
                      some configuration file. *)
                   Lwt_io.printf "Backporting to %s was requested.\n"
                     backport_to
                   >>= fun () ->
                   add_to_column ~bot_info ~backport_to (`PR_ID pr_id)
                     "Request inclusion"
                 else
                   Lwt_io.printf
                     "PR was merged into a branch that is not the backporting \
                      branch nor the master branch.\n" )
      | Error err ->
          Lwt_io.printf "Error: %s\n" err
    else if
      String_utils.string_match ~regexp:"^Backport PR #\\([0-9]*\\):" commit_msg
    then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was backported.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.get_pull_request_cards ~bot_info ~owner:"rocq-prover"
        ~repo:"rocq" ~number:pr_number
      >>= function
      | Ok items -> (
          let backport_to =
            String.chop_prefix_if_exists ~prefix:"refs/heads/" base_ref
          in
          let card_id =
            items |> List.find_map ~f:(function id, 11 -> Some id | _ -> None)
          in
          match card_id with
          | Some card_id ->
              Lwt_io.printlf
                "Pull request rocq-prover/rocq#%d found in project 11. \
                 Updating its fields."
                pr_number
              >>= fun () ->
              add_to_column ~bot_info ~backport_to (`Card_ID card_id) "Shipped"
          | None ->
              (* We could do something in this case, like post a comment to
                 the PR and add the PR to the project. *)
              Lwt_io.printlf
                "Pull request rocq-prover/rocq#%d not found in project 11."
                pr_number )
      | Error e ->
          Lwt_io.printf "%s\n" e
    else Lwt.return_unit
  in
  Lwt_list.iter_s commit_action commits_msg

let apply_after_label ~bot_info ~owner ~repo ~after ~label ~action ~throttle ()
    =
  GitHub_queries.get_open_pull_requests_with_label ~bot_info ~owner ~repo ~label
  >>= function
  | Ok prs ->
      let iter (pr_id, pr_number) =
        GitHub_queries.get_pull_request_label_timeline ~bot_info ~owner ~repo
          ~pr_number
        >>= function
        | Ok timeline ->
            let find (set, name, ts) =
              if set && String.equal name label then Some ts else None
            in
            (* Look for most recent label setting *)
            let timeline = List.rev timeline in
            let days =
              match List.find_map ~f:find timeline with
              | None ->
                  (* even with a race condition it cannot happen *)
                  failwith
                    (f {|Anomaly: Label "%s" absent from timeline of PR #%i|}
                       label pr_number )
              | Some ts ->
                  Utils.days_elapsed ts
            in
            if days >= after then action pr_id pr_number else Lwt.return false
        | Error e ->
            Lwt_io.print (f "Error: %s\n" e) >>= fun () -> Lwt.return false
      in
      Utils.apply_throttle throttle iter prs
  | Error err ->
      Lwt_io.print (f "Error: %s\n" err)

let rocq_check_needs_rebase_pr ~bot_info ~owner ~repo ~warn_after ~close_after
    ~throttle =
  let rebase_label = "needs: rebase" in
  let stale_label = "stale" in
  GitHub_queries.get_label ~bot_info ~owner ~repo ~label:stale_label
  >>= function
  | Ok None ->
      Lwt.return_unit
  | Ok (Some stale_id) ->
      let action pr_id pr_number =
        GitHub_queries.get_pull_request_labels ~bot_info ~owner ~repo ~pr_number
        >>= function
        | Ok labels ->
            let has_label l = List.mem labels ~equal:String.equal l in
            if not (has_label stale_label || has_label "needs: independent fix")
            then
              GitHub_mutations.post_comment ~id:pr_id
                ~message:
                  (f
                     "The \"%s\" label was set more than %i days ago. If the \
                      PR is not rebased in %i days, it will be automatically \
                      closed."
                     rebase_label warn_after close_after )
                ~bot_info
              >>= GitHub_mutations.report_on_posting_comment
              >>= fun () ->
              GitHub_mutations.add_labels ~bot_info ~labels:[stale_id]
                ~issue:pr_id
              >>= fun () -> Lwt.return true
            else Lwt.return false
        | Error err ->
            Lwt_io.print (f "Error: %s\n" err) >>= fun () -> Lwt.return false
      in
      apply_after_label ~bot_info ~owner ~repo ~after:warn_after
        ~label:rebase_label ~action ~throttle ()
  | Error err ->
      Lwt_io.print (f "Error: %s\n" err)

let rocq_check_stale_pr ~bot_info ~owner ~repo ~after ~throttle =
  let label = "stale" in
  let action pr_id _pr_number =
    GitHub_mutations.post_comment ~id:pr_id
      ~message:
        (f
           "This PR was not rebased after %i days despite the warning, it is \
            now closed."
           after )
      ~bot_info
    >>= GitHub_mutations.report_on_posting_comment
    >>= fun () ->
    GitHub_mutations.close_pull_request ~bot_info ~pr_id
    >>= fun () -> Lwt.return true
  in
  apply_after_label ~bot_info ~owner ~repo ~after ~label ~action ~throttle ()

let run_bench ~bot_info ?key_value_pairs comment_info =
  (* Do we want to use this more often? *)
  let open Lwt.Syntax in
  let pr = comment_info.issue in
  let owner = pr.issue.owner in
  let repo = pr.issue.repo in
  let pr_number = pr.number in
  (* We need the GitLab build_id and project_id. Currently there is no good way
     to query this data so we have to jump through some somewhat useless hoops in
     order to get our hands on this information. TODO: do this more directly.*)
  let* gitlab_check_summary =
    GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
      ~number:pr_number
    >>= function
    | Error err ->
        Lwt.return_error
          (f
             "Error while fetching PR refs for %s/%s#%d for running bench job: \
              %s"
             owner repo pr_number err )
    | Ok {base= _; head= {sha= head}} ->
        let head = Str.global_replace (Str.regexp {|"|}) "" head in
        GitHub_queries.get_pipeline_summary ~bot_info ~owner ~repo ~head
  in
  (* Parsing the summary into (build_id, project_id) *)
  let* process_summary =
    match gitlab_check_summary with
    | Error err ->
        Lwt.return_error err
    | Ok summary -> (
      try
        let build_id =
          let regexp =
            f {|.*%s\([0-9]*\)|}
              (Str.quote "[bench](https://gitlab.inria.fr/coq/coq/-/jobs/")
          in
          ( if String_utils.string_match ~regexp summary then
              Str.matched_group 1 summary
            else raise @@ Stdlib.Failure "Could not find GitLab bench job ID" )
          |> Stdlib.int_of_string
        in
        let project_id =
          let regexp = {|.*GitLab Project ID: \([0-9]*\)|} in
          ( if String_utils.string_match ~regexp summary then
              Str.matched_group 1 summary
            else raise @@ Stdlib.Failure "Could not find GitLab Project ID" )
          |> Int.of_string
        in
        Lwt.return_ok (build_id, project_id)
      with Stdlib.Failure s ->
        Lwt.return_error
          (f
             "Error while regexing summary for %s/%s#%d for running bench job: \
              %s"
             owner repo pr_number s ) )
  in
  let* allowed_to_bench =
    GitHub_queries.get_team_membership ~bot_info ~org:"rocq-prover"
      ~team:"contributors" ~user:comment_info.author
  in
  match (allowed_to_bench, process_summary) with
  | Ok true, Ok (build_id, project_id) ->
      (* Permission to bench has been granted *)
      GitLab_mutations.play_job ~bot_info ~gitlab_domain:"gitlab.inria.fr"
        ~project_id ~build_id ?key_value_pairs ()
  | Error err, _ | _, Error err ->
      GitHub_mutations.post_comment ~bot_info ~message:err ~id:pr.id
      >>= GitHub_mutations.report_on_posting_comment
  | Ok false, _ ->
      (* User not found in the team *)
      GitHub_mutations.inform_user_not_in_contributors ~bot_info ~comment_info
