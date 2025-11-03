open Base
open Bot_info
open GitHub_types
open GitLab_types
open Utils
open String_utils
open Lwt.Infix
open Lwt.Syntax

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)

type build_failure = Warn of string | Retry of string | Ignore of string

type artifact_info =
  | ArtifactInfo of
      {artifact_owner: string; artifact_repo: string; artifact_id: string}

type artifact_error =
  | ArtifactEmpty
  | ArtifactContainsMultipleFiles of string list
  | ArtifactDownloadError of string

type run_ci_minimization_error =
  | ArtifactError of
      {url: string; artifact: artifact_info; artifact_error: artifact_error}
  | DownloadError of {url: string; error: string}

type rocq_job_info =
  { docker_image: string
  ; dependencies: string list
  ; targets: string list
  ; compiler: string
  ; opam_variant: string }

module BenchResults = struct
  type t =
    { summary_table: string
    ; failures: string
    ; slow_table: string
    ; slow_number: int
    ; fast_table: string
    ; fast_number: int }
end

type ci_minimization_info =
  { target: string
  ; full_target: string
  ; ci_targets: string list
  ; docker_image: string
  ; opam_switch: string
  ; failing_urls: string
  ; passing_urls: string }

type ci_minimization_job_suggestion_info =
  { base_job_failed: bool
  ; base_job_errored: string option
  ; head_job_succeeded: bool
  ; missing_error: bool
  ; non_v_file: string option
  ; job_kind: string
  ; job_target: string }

type ci_minimization_pr_info =
  { comment_thread_id: GitHub_ID.t
  ; base: string
  ; head: string
  ; pr_number: int
  ; draft: bool
  ; body: string
  ; labels: string list
  ; base_pipeline_finished: bool
  ; head_pipeline_finished: bool
  ; failed_test_suite_jobs: string list }

type ci_minimization_request =
  | Auto
  | RequestSuggested
  | RequestAll
  | RequestExplicit of string list

type ci_minimization_suggestion_kind =
  | Suggested
  | Possible of string
  | Bad of string

type ci_pr_minimization_suggestion =
  | Suggest
  | RunAutomatically
  | Silent of string

(* TODO: This type should move to Minimize_parser once it's created.
   Temporary definition here for compilation until Minimize_parser exists. *)
type minimize_parsed =
  | MinimizeScript of {quote_kind: string; body: string}
  | MinimizeAttachment of {description: string; url: string}

(******************************************************************************)
(* GitLab Trace Processing Utilities                                         *)
(******************************************************************************)

let clean_gitlab_trace trace =
  trace
  |> Str.global_replace (Str.regexp "\027\\[[0-9;]*m") ""
  |> Str.global_replace (Str.regexp "\027\\[0K") ""
  |> Str.global_replace (Str.regexp "section_start:[0-9]*:[a-z_]*\r") ""
  |> Str.global_replace (Str.regexp "section_end:[0-9]*:[a-z_]*\r") ""
  |> String.split_lines

let find_regex_in_lines ~regexps lines =
  List.find_map lines ~f:(fun line ->
      List.find_map regexps ~f:(fun regexp ->
          if string_match ~regexp line then Some (Str.matched_group 1 line)
          else None ) )

let find_all_regex_in_lines ~regexps lines =
  List.filter_map lines ~f:(fun line ->
      List.find_map regexps ~f:(fun regexp ->
          if string_match ~regexp line then Some (Str.matched_group 1 line)
          else None ) )

let trace_action ~repo_full_name trace =
  trace |> String.length
  |> Lwt_io.printlf "Trace size: %d."
  >>= fun () ->
  Lwt.return
    (let test regexp = string_match ~regexp trace in
     if test "Job failed: exit code 137" then Retry "Exit code 137"
     else if test "Job failed: exit status 255" then Retry "Exit status 255"
     else if test "Job failed (system failure)" then Retry "System failure"
     else if
       ( test "Uploading artifacts.*to coordinator... failed"
       || test "Uploading artifacts.*to coordinator... error" )
       && not (test "Uploading artifacts.*to coordinator... ok")
     then Retry "Artifact uploading failure"
     else if
       test "ERROR: Downloading artifacts.*from coordinator... error"
       && test "FATAL: invalid argument"
     then Retry "Artifact downloading failure"
     else if
       test "transfer closed with outstanding read data remaining"
       || test "HTTP request sent, awaiting response... 50[0-9]"
       || test "The requested URL returned error: 502"
       || test "received unexpected HTTP status: 50[0-9]"
       || test "unexpected status from GET request to.*: 50[0-9]"
       || test "error: unable to download 'https://cache.nixos.org/"
       || test "fatal: unable to access .* Couldn't connect to server"
       || test "fatal: unable to access .* Could not resolve host"
       || test "Resolving .* failed: Temporary failure in name resolution"
       || test "unexpected status code .*: 401 Unauthorized"
     then Retry "Connectivity issue"
     else if test "fatal: reference is not a tree" then
       Ignore "Normal failure: pull request was force-pushed."
     else if
       test "fatal: Remote branch pr-[0-9]* not found in upstream origin"
       || test "fatal: [Cc]ouldn't find remote ref refs/heads/pr-"
     then Ignore "Normal failure: pull request was closed."
     else if
       String.equal repo_full_name "coq/coq"
       && test "Error response from daemon: manifest for .* not found"
     then Ignore "Docker image not found. Do not report anything specific."
     else Warn trace )

(******************************************************************************)
(* Pipeline Summary and Error Formatting                                     *)
(******************************************************************************)

let create_pipeline_summary ?summary_top pipeline_info pipeline_url =
  let variables =
    List.map pipeline_info.variables ~f:(fun (key, value) ->
        f "- %s: %s" key value )
    |> String.concat ~sep:"\n"
  in
  let sorted_builds =
    pipeline_info.builds
    |> List.sort ~compare:(fun build1 build2 ->
           String.compare build1.build_name build2.build_name )
  in
  let stage_summary =
    pipeline_info.stages
    |> List.concat_map ~f:(fun stage ->
           sorted_builds
           |> List.filter_map ~f:(fun build ->
                  if String.equal build.stage stage then
                    Some
                      (f "  - [%s](%s/-/jobs/%d)" build.build_name
                         pipeline_info.common_info.http_repo_url build.build_id )
                  else None )
           |> List.cons ("- " ^ stage) )
    |> String.concat ~sep:"\n"
  in
  [ f "This [GitLab pipeline](%s) sets the following variables:" pipeline_url
  ; variables
  ; "It contains the following stages and jobs:"
  ; stage_summary
  ; f "GitLab Project ID: %d" pipeline_info.common_info.project_id ]
  |> (match summary_top with Some text -> List.cons text | None -> Fn.id)
  |> String.concat ~sep:"\n\n"

let run_ci_minimization_error_to_string = function
  | ArtifactError
      { url= artifact_url
      ; artifact= ArtifactInfo {artifact_owner; artifact_repo; artifact_id}
      ; artifact_error } -> (
    match artifact_error with
    | ArtifactEmpty ->
        f "Could not resume minimization with [empty artifact](%s)" artifact_url
    | ArtifactContainsMultipleFiles filenames ->
        f
          "Could not resume minimization because [artifact](%s) contains more \
           than one file: %s"
          artifact_url
          (String.concat ~sep:", " filenames)
    | ArtifactDownloadError error ->
        f
          "Could not resume minimization because [artifact %s/%s:%s](%s) \
           failed to download:\n\
           %s"
          artifact_owner artifact_repo artifact_id artifact_url error )
  | DownloadError {url; error} ->
      f
        "Could not resume minimization because [artifact](%s) failed to \
         download:\n\
         %s"
        url error

(******************************************************************************)
(* CI Minimization Parsing Utilities                                         *)
(******************************************************************************)

let parse_quantity table table_name =
  let regexp = {|.*TOP \([0-9]*\)|} in
  if string_match ~regexp table then
    Str.matched_group 1 table |> Int.of_string |> Lwt.return_ok
  else Lwt.return_error (f "parsing %s table." table_name)

let shorten_ci_check_name target =
  target
  |> Str.global_replace (Str.regexp "GitLab CI job") ""
  |> Str.global_replace (Str.regexp "(pull request)") ""
  |> Str.global_replace (Str.regexp "(branch)") ""
  |> Stdlib.String.trim

let format_options_for_getopts options =
  " " ^ options ^ " " |> Str.global_replace (Str.regexp "[\n\r\t]") " "

let getopts options ~opt =
  map_string_matches
    ~regexp:(f " %s\\(\\.\\|[ =:-]\\|: \\)\\([^ ]+\\) " opt)
    ~f:(fun () -> Str.matched_group 2 options)
    options

let getopt options ~opt =
  options |> getopts ~opt |> List.hd |> Option.value ~default:""

let accumulate_extra_minimizer_arguments options =
  let extra_args = getopts ~opt:"extra-arg" options in
  let inline_stdlib = getopt ~opt:"inline-stdlib" options in
  ( if String.equal inline_stdlib "yes" then Lwt.return ["--inline-coqlib"]
    else
      ( if not (String.equal inline_stdlib "") then
          Lwt_io.printlf
            "Ignoring invalid option to inline-stdlib '%s' not equal to 'yes'"
            inline_stdlib
        else Lwt.return_unit )
      >>= fun () -> Lwt.return_nil )
  >>= fun inline_stdlib_args -> inline_stdlib_args @ extra_args |> Lwt.return

(******************************************************************************)
(* CI Job Info and Benchmark Utilities                                       *)
(******************************************************************************)

let bench_text = function
  | Ok results ->
      (* Formatting helpers *)
      let header2 str = f "## %s" str in
      (* Document *)
      let open BenchResults in
      [ header2 ":checkered_flag: Bench Summary:"
      ; code_wrap results.summary_table
      ; results.failures
      ; header2 @@ f ":turtle: Top %d slow downs:" results.slow_number
      ; results.slow_table
      ; header2 @@ f ":rabbit2: Top %d speed ups:" results.fast_number
      ; results.fast_table ]
      |> String.concat ~sep:"\n" |> Lwt.return
  | Error e ->
      f "Error occured when creating bench summary: %s\n" e |> Lwt.return

(******************************************************************************)
(* GitHub Artifact Parsing                                                   *)
(******************************************************************************)

let parse_github_artifact_url url =
  let github_prefix = "https://github.com/" in
  let regexp =
    Str.quote github_prefix
    ^ "\\([^/]+\\)/\\([^/]+\\)/\\(actions/runs\\|suites\\)/.*/artifacts/\\([0-9]+\\)"
  in
  if string_match ~regexp url then
    Some
      (ArtifactInfo
         { artifact_owner= Str.matched_group 1 url
         ; artifact_repo= Str.matched_group 2 url
         ; artifact_id= Str.matched_group 4 url } )
  else None

(******************************************************************************)
(* CI Status Check Functions                                                 *)
(******************************************************************************)

let send_status_check ~bot_info job_info ~pr_num (gh_owner, gh_repo)
    ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name ~context
    ~failure_reason ~external_id ~trace =
  let job_url =
    f "https://%s/%s/-/jobs/%d" gitlab_domain gitlab_repo_full_name
      job_info.build_id
  in
  let trace_lines = clean_gitlab_trace trace in
  let title, last_index_of_error =
    (* We try to find the line starting with "Error" only in the case
       of an actual script failure. *)
    match failure_reason with
    | "script_failure" ->
        ( "Test has failed on GitLab CI"
        , trace_lines
          |> List.filter_mapi ~f:(fun i line ->
                 if String.is_prefix ~prefix:"Error" line then Some i else None )
          |> List.last )
    | "job_execution_timeout" ->
        ("Test has reached timeout on GitLab CI", None)
    | _ ->
        (failure_reason ^ " on GitLab CI", None)
  in
  let trace_description, short_trace =
    (* If we have a last index of error, we display 40 lines starting
       at the line before (which should include the filename).
       Otherwise, we display only the last 40 lines of the trace *)
    match last_index_of_error with
    | None ->
        ( f
            "We show below the last 40 lines of the trace from GitLab (the \
             complete trace is available [here](%s))."
            job_url
        , trace_lines
          |> Fn.flip List.drop (List.length trace_lines - 40)
          |> String.concat ~sep:"\n" )
    | Some index_of_error ->
        ( f
            "We show below an excerpt from the trace from GitLab starting \
             around the last detected \"Error\" (the complete trace is \
             available [here](%s))."
            job_url
        , trace_lines
          |> Fn.flip List.drop (index_of_error - 1)
          |> Fn.flip List.take 40 |> String.concat ~sep:"\n" )
  in
  let rocq_job_info =
    let open Option in
    find_regex_in_lines
      ~regexps:
        [ "^Using Docker executor with image \\([^ ]+\\)"
        ; "options=Options(docker='\\([^']+\\)')" ]
      trace_lines
    >>= fun docker_image ->
    let dependencies =
      find_all_regex_in_lines
        ~regexps:["^Downloading artifacts for \\([^ ]+\\)"]
        trace_lines
    in
    (* The CI script prints "CI_TARGETS=foo bar" through "env" if it is non-default,
       then "CI_TARGETS = foo bar" even if it is the default (from job name).
       We use the later. *)
    find_regex_in_lines ~regexps:["^CI_TARGETS = \\(.*\\)"] trace_lines
    >>= fun targets ->
    let targets = String.split ~on:' ' targets in
    find_regex_in_lines ~regexps:["^COMPILER=\\(.*\\)"] trace_lines
    >>= fun compiler ->
    find_regex_in_lines ~regexps:["^OPAM_VARIANT=\\(.*\\)"] trace_lines
    >>= fun opam_variant ->
    Some {docker_image; dependencies; targets; compiler; opam_variant}
  in
  let* summary_tail_prefix =
    match rocq_job_info with
    | Some {docker_image; dependencies; targets; compiler; opam_variant} ->
        let switch_name = compiler ^ opam_variant in
        let dependencies = String.concat ~sep:"` `" dependencies in
        let targets = String.concat ~sep:"` `" targets in
        Lwt.return
          (f
             "This job ran on the Docker image `%s` with OCaml `%s` and \
              depended on jobs `%s`. It built targets `%s`.\n\n"
             docker_image switch_name dependencies targets )
    | None ->
        Lwt.return ""
  in
  let summary_tail = summary_tail_prefix ^ trace_description in
  let text = "```\n" ^ short_trace ^ "\n```" in
  if job_info.allow_fail then
    Lwt_io.printf "Job is allowed to fail.\n"
    <&> ( match bot_info.github_install_token with
        | None ->
            (* Allow failure messages are reported with the Checks API only. *)
            Lwt.return_unit
        | Some _ -> (
            GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
              ~repo:gh_repo
            >>= function
            | Ok repo_id ->
                let open Lwt.Syntax in
                let+ _ =
                  GitHub_mutations.create_check_run ~bot_info ~name:context
                    ~repo_id ~head_sha:job_info.common_info.head_commit
                    ~conclusion:NEUTRAL ~status:COMPLETED ~title
                    ~details_url:job_url
                    ~summary:("This job is allowed to fail.\n\n" ^ summary_tail)
                    ~text ~external_id ()
                in
                ()
            | Error e ->
                Lwt_io.printf "No repo id: %s\n" e ) )
    <&>
    (* If we are in a PR branch, we can post a comment. *)
    if String.equal job_info.build_name "library:ci-fiat_crypto_legacy" then
      let message =
        f "The job [%s](%s) has failed in allow failure mode\nping @JasonGross"
          job_info.build_name job_url
      in
      match pr_num with
      | Some number -> (
          GitHub_queries.get_pull_request_refs ~bot_info ~owner:gh_owner
            ~repo:gh_repo ~number
          >>= function
          | Ok {issue= id; head}
          (* Commits reported back by get_pull_request_refs are surrounded in double quotes *)
            when String.equal head.sha
                   (f {|"%s"|} job_info.common_info.head_commit) ->
              GitHub_mutations.post_comment ~bot_info ~id ~message
              >>= GitHub_mutations.report_on_posting_comment
          | Ok {head} ->
              Lwt_io.printf
                "We are on a PR branch but the commit (%s) is not the current \
                 head of the PR (%s). Doing nothing.\n"
                job_info.common_info.head_commit head.sha
          | Error err ->
              Lwt_io.printf
                "Couldn't get a database id for %s#%d because the following \
                 error occured:\n\
                 %s\n"
                gitlab_repo_full_name number err )
      | None ->
          Lwt_io.printf "We are not on a PR branch. Doing nothing.\n"
    else Lwt.return_unit
  else
    Lwt_io.printf "Pushing a status check...\n"
    <&>
    match bot_info.github_install_token with
    | None ->
        GitHub_mutations.send_status_check ~repo_full_name:github_repo_full_name
          ~commit:job_info.common_info.head_commit ~state:"failure" ~url:job_url
          ~context ~description:title ~bot_info
    | Some _ -> (
        GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner ~repo:gh_repo
        >>= function
        | Ok repo_id ->
            let open Lwt.Syntax in
            let+ _ =
              GitHub_mutations.create_check_run ~bot_info ~name:context ~repo_id
                ~head_sha:job_info.common_info.head_commit ~conclusion:FAILURE
                ~status:COMPLETED ~title ~details_url:job_url
                ~summary:
                  ( "This job has failed. If you need to, you can restart it \
                     directly in the GitHub interface using the \"Re-run\" \
                     button.\n\n" ^ summary_tail )
                ~text ~external_id ()
            in
            ()
        | Error e ->
            Lwt_io.printf "No repo id: %s\n" e )

let inform_user_not_in_contributors ~bot_info ~comment_info =
  GitHub_mutations.post_comment ~bot_info ~id:comment_info.issue.id
    ~message:
      (f
         "Sorry, @%s, I only accept requests from members of the \
          `@rocq-prover/contributors` team. If you are a regular contributor, \
          you can request to join the team by asking any core developer."
         comment_info.author )
  >>= GitHub_mutations.report_on_posting_comment

(******************************************************************************)
(* CI Minimization Core Functions                                            *)
(******************************************************************************)

(* For grammatical correctness, all messages are expected to follow "because" *)
let ci_minimization_suggest ~base
    { base_job_failed
    ; base_job_errored
    ; head_job_succeeded
    ; missing_error
    ; non_v_file
    ; job_kind
    ; job_target } =
  if head_job_succeeded then Bad "job succeeded!"
  else if missing_error then Bad "no error message was found"
  else
    match (base_job_errored, non_v_file) with
    | _, Some filename ->
        Bad (f "error message did not occur in a .v file (%s)" filename)
    | Some err, _ ->
        Possible (f "base job at %s errored with message %s" base err)
    | None, None ->
        if base_job_failed then Possible (f "base job at %s failed" base)
        else if
          not (List.exists ~f:(String.equal job_kind) ["library"; "plugin"])
        then
          Possible
            (f "the job is a %s which is not a library nor a plugin" job_kind)
        else if String.equal job_target "ci-coq_tools" then
          Possible
            (f
               "coq-tools is too sensitive to the output of coqc to be \
                minimized at this time (instead, @JasonGross can help diagnose \
                and fix the issue)" )
        else Suggested

let suggest_ci_minimization_for_pr = function
  (* don't suggest if there are failed test-suite jobs (TODO: decide about async?) *)
  | {failed_test_suite_jobs= _ :: _ as failed_test_suite_jobs} ->
      Silent
        (f "the following test-suite jobs failed: %s"
           (String.concat ~sep:", " failed_test_suite_jobs) )
  (* This next case is a dummy case so OCaml doesn't complain about us
     never using RunAutomatically; we should probably remove it when
     we add a criterion for running minimization automatically *)
  | {labels}
    when List.exists ~f:(String.equal "coqbot request ci minimization") labels
    ->
      RunAutomatically
  | {labels} when List.exists ~f:(String.equal "kind: infrastructure") labels ->
      Silent "this PR is labeled with kind: infrastructure"
  | {body} when not (String.is_substring ~substring:"offer-minimizer: on" body)
    ->
      Silent
        "the PR body does not contain an 'offer-minimizer: on' directive, \
         which turns on minimization suggestions"
  | {draft= true} ->
      Suggest
  | _ ->
      Suggest

let run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo ~pr_number
    ~base ~head ~ci_minimization_infos ~bug_file ~minimizer_extra_arguments =
  let open Lwt_result.Infix in
  (* for convenience of control flow, we always create the temporary
     file, but we only pass in the file name if the bug file contents
     is non-None *)
  Lwt_io.with_temp_file (fun (bug_file_name, bug_file_ch) ->
      (let open Lwt.Infix in
       match bug_file with
       | None ->
           Lwt.return_ok ()
       | Some (MinimizeScript {body= bug_file_contents}) ->
           Lwt_io.write bug_file_ch bug_file_contents >>= Lwt.return_ok
       | Some (MinimizeAttachment {url}) -> (
         match parse_github_artifact_url url with
         | Some
             ( ArtifactInfo {artifact_owner; artifact_repo; artifact_id} as
               artifact ) -> (
             Lwt_io.printlf
               "Downloading artifact %s/%s:%s for %s/%s#%s (%s) (parsed from \
                %s)"
               artifact_owner artifact_repo artifact_id owner repo pr_number
               (GitHub_ID.to_string comment_thread_id)
               url
             >>= fun () ->
             GitHub_queries.get_artifact_blob ~bot_info ~owner:artifact_owner
               ~repo:artifact_repo ~artifact_id
             >>= function
             | Ok [(_filename, bug_file_contents)] ->
                 Lwt_io.write bug_file_ch bug_file_contents >>= Lwt.return_ok
             | Ok [] ->
                 Lwt.return_error
                   (ArtifactError {url; artifact; artifact_error= ArtifactEmpty})
             | Ok files ->
                 files
                 |> List.map ~f:(fun (filename, _contents) -> filename)
                 |> fun artifact_filenames ->
                 Lwt.return_error
                   (ArtifactError
                      { url
                      ; artifact
                      ; artifact_error=
                          ArtifactContainsMultipleFiles artifact_filenames } )
             | Error message ->
                 Lwt.return_error
                   (ArtifactError
                      { url
                      ; artifact
                      ; artifact_error= ArtifactDownloadError message } ) )
         | None ->
             HTTP_utils.download_to ~uri:(Uri.of_string url) bug_file_ch
             |> Lwt_result.map_error (fun error -> DownloadError {url; error}) )
      )
      >>= fun () ->
      let open Lwt.Infix in
      Lwt_io.flush bug_file_ch
      >>= fun () ->
      let bug_file_name = Option.map ~f:(fun _ -> bug_file_name) bug_file in
      Lwt_list.map_s
        (fun { target
             ; ci_targets
             ; opam_switch
             ; failing_urls
             ; passing_urls
             ; docker_image } ->
          Git_utils.git_run_ci_minimization ~bot_info ~comment_thread_id ~owner
            ~repo ~pr_number ~docker_image ~ci_targets ~target ~opam_switch
            ~failing_urls ~passing_urls ~base ~head ~minimizer_extra_arguments
            ~bug_file_name
          >>= fun result -> Lwt.return (target, result) )
        ci_minimization_infos
      >>= Lwt.return_ok )
  >>= fun results ->
  results
  |> List.partition_map ~f:(function
       | target, Ok () ->
           Either.First target
       | target, Error f ->
           Either.Second (target, f) )
  |> Lwt.return_ok

let ci_minimization_extract_job_specific_info ~head_pipeline_summary
    ~base_pipeline_summary ~base_checks_errors ~base_checks = function
  | ( {name= full_name; summary= Some summary; text= Some text}
    , head_job_succeeded ) ->
      let base_job_errored =
        List.find_map
          ~f:(fun (base_name, err) ->
            if String.equal full_name base_name then Some err else None )
          base_checks_errors
      in
      let base_job_failed =
        List.exists
          ~f:(fun ({name= base_name}, success_base) ->
            String.equal full_name base_name && not success_base )
          base_checks
      in
      if string_match ~regexp:"\\([^: ]*\\):\\(ci-[A-Za-z0-9_-]*\\)" full_name
      then
        let name = Str.matched_group 0 full_name in
        let job_kind = Str.matched_group 1 full_name in
        let target = Str.matched_group 2 full_name in
        let extract_artifact_url job_name summary =
          if
            string_match
              ~regexp:(f "\\[%s\\](\\([^)]+\\))" (Str.quote job_name))
              summary
          then Some (Str.matched_group 1 summary ^ "/artifacts/download")
          else None
        in
        let collapse_summary name summary =
          f "<details><summary>%s</summary>\n\n%s\n</details>\n" name summary
        in
        if
          string_match
            ~regexp:
              "This job ran on the Docker image `\\([^`]+\\)` with OCaml \
               `\\([^`]+\\)` and depended on jobs \\(\\(`[^`]+` ?\\)+\\). It \
               built targets \\(\\(`[^`]+` ?\\)+\\).\n\n"
            summary
        then
          let docker_image, opam_switch, dependencies, targets =
            ( Str.matched_group 1 summary
            , Str.matched_group 2 summary
            , Str.matched_group 3 summary
            , Str.matched_group 5 summary )
          in
          let dependencies = Str.split (Str.regexp "[ `]+") dependencies in
          let ci_targets = Str.split (Str.regexp "[ `]+") targets in
          let missing_error, non_v_file =
            if
              string_match
                ~regexp:
                  "\n\
                   File \"\\([^\"]*\\)\", line [0-9]*, characters [0-9]*-[0-9]*:\n\
                   Error:"
                text
            then
              let filename = Str.matched_group 1 text in
              ( false
              , if String.is_suffix ~suffix:".v" filename then None
                else Some filename )
            else (true, None)
          in
          let extract_artifacts url =
            List.partition_map
              ~f:(fun name ->
                match extract_artifact_url name url with
                | Some v ->
                    First v
                | None ->
                    Second name )
              (name :: dependencies)
          in
          match
            ( extract_artifacts base_pipeline_summary
            , extract_artifacts head_pipeline_summary )
          with
          | (base_urls, []), (head_urls, []) ->
              Ok
                ( { base_job_failed
                  ; base_job_errored
                  ; missing_error
                  ; non_v_file
                  ; job_kind
                  ; head_job_succeeded
                  ; job_target= target }
                , { target
                  ; full_target= name
                  ; ci_targets
                  ; docker_image
                  ; opam_switch
                  ; failing_urls= String.concat ~sep:" " head_urls
                  ; passing_urls= String.concat ~sep:" " base_urls } )
          | (_, (_ :: _ as base_failed)), _ ->
              Error
                (f "Could not find base dependencies artifacts for %s in:\n%s"
                   (String.concat ~sep:" " base_failed)
                   (collapse_summary "Base Pipeline Summary"
                      base_pipeline_summary ) )
          | _, (_, (_ :: _ as head_failed)) ->
              Error
                (f "Could not find head dependencies artifacts for %s in:\n%s"
                   (String.concat ~sep:" " head_failed)
                   (collapse_summary "Head Pipeline Summary"
                      head_pipeline_summary ) )
        else
          Error
            (f "Could not find needed parameters for job %s in summary:\n%s\n"
               name
               (collapse_summary "Summary" summary) )
      else
        Error (f "Could not separate '%s' into job_kind:ci-target." full_name)
  | {name; summary= None}, _ ->
      Error (f "Could not find summary for job %s." name)
  | {name; text= None}, _ ->
      Error (f "Could not find text for job %s." name)

let fetch_ci_minimization_info ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary ?base_sha ?head_sha () =
  let open Lwt.Syntax in
  let* () =
    Lwt_io.printlf "I'm going to look for failed tests to minimize on PR #%d."
      pr_number
  in
  let* refs =
    match (base_sha, head_sha) with
    | None, _ | _, None ->
        let open Lwt_result.Syntax in
        let+ {base= {sha= base}; head= {sha= head}} =
          GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
            ~number:pr_number
        in
        (base, head)
    | Some base, Some head ->
        Lwt.return_ok (base, head)
  in
  match refs with
  | Error err ->
      Lwt.return_error
        ( None
        , f "Error while fetching PR refs for %s/%s#%d for CI minimization: %s"
            owner repo pr_number err )
  | Ok (base, head) -> (
      (* TODO: figure out why there are quotes, cf https://github.com/rocq-prover/bot/issues/61 *)
      let base = Str.global_replace (Str.regexp {|"|}) "" base in
      let head = Str.global_replace (Str.regexp {|"|}) "" head in
      GitHub_queries.get_base_and_head_checks ~bot_info ~owner ~repo ~pr_number
        ~base ~head
      >>= function
      | Error err ->
          Lwt.return_error
            ( None
            , f "Error while looking for failed library tests to minimize: %s"
                err )
      | Ok {pr_id; base_checks; head_checks; draft; body; labels} -> (
          let partition_errors =
            List.partition_map ~f:(function
              | Error (name, error) ->
                  Either.First (shorten_ci_check_name name, error)
              | Ok (result, status) ->
                  Either.Second
                    ( {result with name= shorten_ci_check_name result.name}
                    , status ) )
          in
          let base_checks_errors, base_checks = partition_errors base_checks in
          let head_checks_errors, head_checks = partition_errors head_checks in
          head_checks_errors
          |> Lwt_list.iter_p (fun (_, error) ->
                 Lwt_io.printlf
                   "Non-fatal error while looking for failed tests of PR #%d \
                    to minimize: %s"
                   pr_number error )
          >>= fun () ->
          let extract_pipeline_check =
            List.partition3_map ~f:(fun (check_tab_info, success) ->
                if
                  String.is_prefix ~prefix:"GitLab CI pipeline"
                    check_tab_info.name
                then `Fst (check_tab_info, Option.is_some success)
                else
                  match success with
                  | Some success ->
                      `Snd (check_tab_info, success)
                  | None ->
                      `Trd check_tab_info )
          in
          let extract_pipeline_check_errors =
            List.filter ~f:(fun (name, _) ->
                String.is_prefix ~prefix:"GitLab CI pipeline" name )
          in
          match
            ( ( extract_pipeline_check base_checks
              , extract_pipeline_check_errors base_checks_errors )
            , ( (head_pipeline_summary, true)
              , extract_pipeline_check head_checks
              , extract_pipeline_check_errors head_checks_errors ) )
          with
          | ( ( ( [ ( {summary= Some base_pipeline_summary}
                    , base_pipeline_finished ) ]
                , base_checks
                , unfinished_base_checks )
              , _base_pipeline_checks_errors )
            , ( ( (Some head_pipeline_summary, head_pipeline_finished)
                , (_, head_checks, unfinished_head_checks)
                , _head_pipeline_checks_errors )
              | ( (None, _)
                , ( [ ( {summary= Some head_pipeline_summary}
                      , head_pipeline_finished ) ]
                  , head_checks
                  , unfinished_head_checks )
                , _head_pipeline_checks_errors ) ) ) ->
              Lwt_io.printf
                "Looking for failed tests to minimize among %d head checks (%d \
                 base checks) (head checks: %s) (unfinished head checks: %s) \
                 (base checks: %s) (unfinished base checks: %s).\n"
                (List.length head_checks) (List.length base_checks)
                ( head_checks
                |> List.map ~f:(fun ({name}, _) -> name)
                |> String.concat ~sep:", " )
                ( unfinished_head_checks
                |> List.map ~f:(fun {name} -> name)
                |> String.concat ~sep:", " )
                ( base_checks
                |> List.map ~f:(fun ({name}, _) -> name)
                |> String.concat ~sep:", " )
                ( unfinished_base_checks
                |> List.map ~f:(fun {name} -> name)
                |> String.concat ~sep:", " )
              >>= fun () ->
              let failed_test_suite_jobs =
                List.filter_map head_checks ~f:(fun ({name}, success) ->
                    if string_match ~regexp:"test-suite" name && not success
                    then Some name
                    else None )
                @ List.filter_map head_checks_errors ~f:(fun (name, _) ->
                      if string_match ~regexp:"test-suite" name then Some name
                      else None )
              in
              let possible_jobs_to_minimize, unminimizable_jobs =
                head_checks
                |> List.partition_map ~f:(fun (({name}, _) as head_check) ->
                       match
                         ci_minimization_extract_job_specific_info
                           ~head_pipeline_summary ~base_pipeline_summary
                           ~base_checks_errors ~base_checks head_check
                       with
                       | Error err ->
                           Either.Second (name, err)
                       | Ok result ->
                           Either.First result )
              in
              let unminimizable_jobs =
                unminimizable_jobs
                @ ( unfinished_head_checks
                  |> List.map ~f:(fun {name} ->
                         (name, f "Job %s is still in progress." name) ) )
              in
              Lwt.return_ok
                ( { comment_thread_id= pr_id
                  ; base
                  ; head
                  ; pr_number
                  ; draft
                  ; body
                  ; labels
                  ; base_pipeline_finished
                  ; head_pipeline_finished
                  ; failed_test_suite_jobs }
                , possible_jobs_to_minimize
                , unminimizable_jobs )
          | ((_, _, _), _), ((None, _), ([({summary= None}, _)], _, _), _) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check summary for head commit %s \
                     and no summary was passed."
                    head )
          | ((_, _, _), _), ((None, _), ([], _, _), [])
            when List.is_empty head_checks_errors ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check for head commit %s and no \
                     summary was passed.  (Found checks: %s)"
                    head
                    ( head_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) )
          | ((_, _, _), _), ((None, _), ([], _, _), []) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check for head commit %s and no \
                     summary was passed.  (Found checks: %s) (Errored while \
                     finding checks: %s)"
                    head
                    ( head_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " )
                    ( head_checks_errors
                    |> List.map ~f:(fun (name, _) -> name)
                    |> String.concat ~sep:", " ) )
          | ( ((_, _, _), _)
            , ((None, _), ([], _, _), [(_head_check_name, head_check_error)]) )
            ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not successfully find pipeline check for head \
                     commit %s and no summary was passed. Error: %s"
                    head head_check_error )
          | ( ((_, _, _), _)
            , ( (None, _)
              , ([], _, _)
              , (_ :: _ :: _ as head_pipeline_checks_errors) ) ) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not successfully find pipeline check for head \
                     commit %s and no summary was passed. Found multiple \
                     errors on head pipeline checks:\n\
                     %s"
                    head
                    ( head_pipeline_checks_errors
                    |> List.map ~f:(fun (name, error) ->
                           f "- %s: %s" name error )
                    |> String.concat ~sep:"\n" ) )
          | ( ((_, _, _), _)
            , ((None, _), ((_ :: _ :: _ as pipeline_head_checks), _, _), _) ) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Found several pipeline checks instead of one for head \
                     commit %s and no summary was passed.  (Found checks: %s)"
                    head
                    ( pipeline_head_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) )
          | (([({summary= None}, _)], _, _), _), ((_, _), (_, _, _), _) ->
              Lwt.return_error
                ( Some pr_id
                , f "Could not find pipeline check summary for base commit %s."
                    base )
          | (([], _, _), []), ((_, _), (_, _, _), _)
            when List.is_empty base_checks_errors ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check for base commit %s.  (Found \
                     checks: %s)"
                    base
                    ( base_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) )
          | (([], _, _), []), ((_, _), (_, _, _), _) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check for base commit %s.  (Found \
                     checks: %s) (Errored while finding checks: %s)"
                    base
                    ( base_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " )
                    ( base_checks_errors
                    |> List.map ~f:(fun (name, _) -> name)
                    |> String.concat ~sep:", " ) )
          | ( ( ([], _, _)
              , [(_base_pipeline_check_name, base_pipeline_check_error)] )
            , ((_, _), (_, _, _), _) ) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not successfully find pipeline check for base \
                     commit %s.  Error: %s"
                    base base_pipeline_check_error )
          | ( (([], _, _), (_ :: _ :: _ as base_pipeline_checks_errors))
            , ((_, _), (_, _, _), _) ) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not successfully find pipeline check for base \
                     commit %s. Found multiple errors on base pipeline checks:\n\
                     %s"
                    base
                    ( base_pipeline_checks_errors
                    |> List.map ~f:(fun (name, error) ->
                           f "- %s: %s" name error )
                    |> String.concat ~sep:"\n" ) )
          | ( (((_ :: _ :: _ as pipeline_base_checks), _, _), _)
            , ((_, _), (_, _, _), _) ) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Found several pipeline checks instead of one for base \
                     commit %s.  (Found checks: %s)"
                    base
                    ( pipeline_base_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) ) ) )

let minimize_failed_tests ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary ~request ~comment_on_error ~bug_file ~options
    ?base_sha ?head_sha () =
  let options = format_options_for_getopts options in
  accumulate_extra_minimizer_arguments options
  >>= fun minimizer_extra_arguments ->
  Lwt_io.printlf
    "Parsed options for the bug minimizer at %s/%s#%d from '%s' into \
     {minimizer_extra_arguments: '%s'}"
    owner repo pr_number options
    (String.concat ~sep:" " minimizer_extra_arguments)
  >>= fun () ->
  fetch_ci_minimization_info ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary ?base_sha ?head_sha ()
  >>= function
  | Ok
      ( ( { comment_thread_id
          ; base
          ; head
          ; base_pipeline_finished
          ; head_pipeline_finished } as ci_minimization_pr_info )
      , possible_jobs_to_minimize
      , unminimizable_jobs ) -> (
      let compare_minimization_info {target= target1} {target= target2} =
        String.compare target1 target2
      in
      let unminimizable_jobs =
        unminimizable_jobs
        |> List.sort ~compare:(fun (name1, _) (name2, _) ->
               String.compare name1 name2 )
      in
      possible_jobs_to_minimize
      |> List.sort ~compare:(fun (_, info1) (_, info2) ->
             compare_minimization_info info1 info2 )
      |> List.map ~f:(fun (suggestion_info, minimization_info) ->
             (ci_minimization_suggest ~base suggestion_info, minimization_info) )
      |> List.partition3_map ~f:(function
           | Suggested, minimization_info ->
               `Fst minimization_info
           | Possible reason, minimization_info ->
               `Snd (reason, minimization_info)
           | Bad reason, minimization_info ->
               `Trd (reason, minimization_info) )
      |> fun ( suggested_jobs_to_minimize
             , possible_jobs_to_minimize
             , bad_jobs_to_minimize ) ->
      let suggested_and_possible_jobs_to_minimize =
        suggested_jobs_to_minimize
        @ List.map
            ~f:(fun (_, minimization_info) -> minimization_info)
            possible_jobs_to_minimize
        |> List.sort ~compare:compare_minimization_info
      in
      let jobs_to_minimize, suggest_minimization =
        match
          (request, suggest_ci_minimization_for_pr ci_minimization_pr_info)
        with
        | Auto, RunAutomatically
          when base_pipeline_finished && head_pipeline_finished ->
            (suggested_jobs_to_minimize, Ok ())
        | Auto, RunAutomatically ->
            (* XXX TODO: What should we do in the "run automatically case" when base or head pipeline has not finished? *)
            (suggested_jobs_to_minimize, Ok ())
        | Auto, Suggest ->
            ([], Ok ())
        | Auto, Silent reason ->
            ([], Error reason)
        | RequestSuggested, (RunAutomatically | Suggest) ->
            (suggested_jobs_to_minimize, Ok ())
        | RequestSuggested, Silent reason ->
            (suggested_jobs_to_minimize, Error reason)
        | RequestAll, _ ->
            ( suggested_and_possible_jobs_to_minimize
            , Error "all minimizable jobs were already requested" )
        | RequestExplicit requests, _ ->
            ( suggested_and_possible_jobs_to_minimize
              |> List.filter ~f:(fun {target} ->
                     List.exists
                       ~f:(fun request -> String.equal target request)
                       requests )
            , Error "the user requested an explicit list of jobs" )
      in
      ( match jobs_to_minimize with
      | [] ->
          Lwt_io.printlf
            "Found no jobs to initiate CI minimization on for PR #%d" pr_number
      | _ ->
          Lwt_io.printlf "Initiating CI minimization for PR #%d on jobs: %s"
            pr_number
            ( jobs_to_minimize
            |> List.map ~f:(fun {target} -> target)
            |> String.concat ~sep:", " ) )
      >>= fun () ->
      run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo
        ~pr_number:(Int.to_string pr_number) ~base ~head
        ~ci_minimization_infos:jobs_to_minimize ~minimizer_extra_arguments
        ~bug_file
      >>= function
      | Ok (jobs_minimized, jobs_that_could_not_be_minimized) -> (
          let pluralize word ?plural ls =
            match (ls, plural) with
            | [_], _ ->
                word
            | _, Some plural ->
                plural
            | _, _ ->
                word ^ "s"
          in
          (* Construct a comment body *)
          let unminimizable_jobs_description ~f =
            match
              unminimizable_jobs |> List.filter ~f:(fun (name, _) -> f name)
            with
            | [] ->
                None
            | [(name, err)] ->
                Some
                  (Printf.sprintf
                     "The job %s could not be minimized because %s.\n" name err )
            | unminimizable_jobs ->
                Some
                  ( "The following jobs could not be minimized:\n"
                  ^ ( unminimizable_jobs
                    |> List.map ~f:(fun (name, err) ->
                           Printf.sprintf "- %s (%s)" name err )
                    |> String.concat ~sep:"\n" )
                  ^ "\n\n" )
          in
          let bad_jobs_description ~f =
            match
              bad_jobs_to_minimize
              |> List.filter ~f:(fun (_, {target}) -> f target)
            with
            | [] ->
                None
            | [(reason, {target})] ->
                Some
                  (Printf.sprintf "The job %s was not minimized because %s.\n"
                     target reason )
            | bad_jobs ->
                Some
                  ( "The following jobs were not minimized:\n"
                  ^ ( bad_jobs
                    |> List.map ~f:(fun (reason, {target}) ->
                           Printf.sprintf "- %s because %s" target reason )
                    |> String.concat ~sep:"\n" )
                  ^ "\n\n" )
          in
          let bad_and_unminimizable_jobs_description ~f =
            match
              (bad_jobs_description ~f, unminimizable_jobs_description ~f)
            with
            | None, None ->
                None
            | Some msg, None | None, Some msg ->
                Some msg
            | Some msg1, Some msg2 ->
                Some (msg1 ^ msg2)
          in
          let failed_minimization_description =
            match jobs_that_could_not_be_minimized with
            | [] ->
                None
            | _ :: _ ->
                Some
                  ( "I failed to trigger minimization on the following jobs:\n"
                  ^ ( jobs_that_could_not_be_minimized
                    |> List.map ~f:(fun (name, err) ->
                           Printf.sprintf "- %s (%s)" name err )
                    |> String.concat ~sep:"\n" )
                  ^ "\n\n" )
          in
          let unfinished_pipelines_description =
            (if base_pipeline_finished then [] else [f "base commit (%s)" base])
            @ if head_pipeline_finished then [] else [f "head commit (%s)" head]
          in
          let try_again_msg =
            match unfinished_pipelines_description with
            | [] ->
                ""
            | ls ->
                f
                  "\n\
                   However, you may want to try again once the %s for the %s \
                   %s."
                  (pluralize "pipeline" ls)
                  (ls |> String.concat ~sep:" and ")
                  (pluralize "finishes" ~plural:"finish" ls)
          in
          let may_wish_to_wait_msg =
            match unfinished_pipelines_description with
            | [] ->
                ""
            | ls ->
                f
                  "\n\n\
                   :warning: :hourglass: You may want to wait until the %s for \
                   the %s %s."
                  (pluralize "pipeline" ls)
                  (ls |> String.concat ~sep:" and ")
                  (pluralize "finishes" ~plural:"finish" ls)
          in
          let note_some_head_unfinished_msg =
            if head_pipeline_finished then ""
            else
              f
                "\n\
                 Some jobs may have been missed because the pipeline for the \
                 head commit (%s) has not yet finished."
                head
          in
          let note_some_base_unfinished_msg =
            if base_pipeline_finished then ""
            else
              f
                "\n\
                 However, minimization may fail because the pipeline for the \
                 base commit (%s) has not yet finished."
                base
          in
          ( match (request, jobs_minimized, failed_minimization_description) with
          | RequestAll, [], None ->
              Lwt.return_some
                ( match
                    bad_and_unminimizable_jobs_description ~f:(fun _ -> true)
                  with
                | None ->
                    f "No valid CI jobs detected for %s.%s" head try_again_msg
                | Some msg ->
                    f
                      "I attempted to run all CI jobs at commit %s for \
                       minimization, but was unable to find any jobs to \
                       minimize.%s\n\n\
                       %s"
                      head try_again_msg msg )
          | RequestAll, _, _ ->
              ( match
                  bad_and_unminimizable_jobs_description ~f:(fun _ -> true)
                with
              | Some msg ->
                  Lwt_io.printlf
                    "When attempting to run CI Minimization by request all on \
                     %s/%s@%s for PR #%d:\n\
                     %s"
                    owner repo head pr_number msg
              | None ->
                  Lwt.return_unit )
              >>= fun () ->
              ( match jobs_minimized with
              | [] ->
                  f
                    "I did not succeed at triggering minimization on any jobs \
                     at commit %s.%s"
                    head try_again_msg
              | _ :: _ ->
                  (* TODO: change https://github.com/rocq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
                  f
                    "I am now [%s \
                     minimization](https://github.com/rocq-community/run-coq-bug-minimizer/actions) \
                     at commit %s on %s. I'll come back to you with the \
                     results once it's done.%s"
                    (if Option.is_none bug_file then "running" else "resuming")
                    head
                    (jobs_minimized |> String.concat ~sep:", ")
                    note_some_head_unfinished_msg )
              ^ "\n\n"
              ^ Option.value ~default:"" failed_minimization_description
              |> Lwt.return_some
          | RequestExplicit requests, _, _ ->
              (* N.B. requests may be things like library:ci-cross_crypto,
                 while the job targets are things like GitLab CI job
                 library:ci-cross_crypto (pull request) *)
              requests
              |> List.partition3_map ~f:(fun request ->
                     match
                       ( List.exists
                           ~f:(string_match ~regexp:(Str.quote request))
                           jobs_minimized
                       , List.find
                           ~f:(fun (target, _) ->
                             string_match ~regexp:(Str.quote request) target )
                           jobs_that_could_not_be_minimized
                       , List.find
                           ~f:(fun (target, _) ->
                             string_match ~regexp:(Str.quote request) target )
                           unminimizable_jobs
                       , List.find
                           ~f:(fun (_, {target}) ->
                             string_match ~regexp:(Str.quote request) target )
                           bad_jobs_to_minimize )
                     with
                     | true, _, _, _ ->
                         `Fst request
                     | false, Some (target, err), _, _ ->
                         `Snd
                           (f "%s: failed to trigger minimization (%s)" target
                              err )
                     | false, None, Some (target, err), _ ->
                         `Snd (f "%s could not be minimized (%s)" target err)
                     | false, None, None, Some (reason, {target}) ->
                         `Snd
                           (f "%s was not minimized because %s" target reason)
                     | false, None, None, None ->
                         `Trd request )
              |> fun ( successful_requests
                     , unsuccessful_requests
                     , unfound_requests ) ->
              let unsuccessful_requests_report =
                match unsuccessful_requests with
                | [] ->
                    None
                | [msg] ->
                    Some msg
                | _ ->
                    Some
                      ( "The following requests were not fulfilled:\n"
                      ^ ( unsuccessful_requests
                        |> List.map ~f:(fun msg -> "- " ^ msg)
                        |> String.concat ~sep:"\n" )
                      ^ "\n\n" )
              in
              let unfound_requests_report =
                let all_jobs =
                  List.map
                    ~f:(fun (target, _) -> target)
                    jobs_that_could_not_be_minimized
                  @ List.map ~f:(fun (target, _) -> target) unminimizable_jobs
                  @ List.map
                      ~f:(fun (_, {target}) -> target)
                      bad_jobs_to_minimize
                  |> List.sort ~compare:String.compare
                in
                match unfound_requests with
                | [] ->
                    None
                | [request] ->
                    Some
                      (f
                         "requested target '%s' could not be found among the \
                          jobs %s.%s"
                         request
                         (all_jobs |> String.concat ~sep:", ")
                         note_some_head_unfinished_msg )
                | _ :: _ :: _ ->
                    Some
                      (f
                         "requested targets %s could not be found among the \
                          jobs %s.%s"
                         (unfound_requests |> String.concat ~sep:", ")
                         (all_jobs |> String.concat ~sep:", ")
                         note_some_head_unfinished_msg )
              in
              let unsuccessful_requests_report =
                match
                  (unsuccessful_requests_report, unfound_requests_report)
                with
                | None, None ->
                    None
                | Some msg, None ->
                    Some msg
                | None, Some msg ->
                    Some ("The " ^ msg)
                | Some msg1, Some msg2 ->
                    Some (msg1 ^ "\nAdditionally, the " ^ msg2)
              in
              ( match (successful_requests, unsuccessful_requests_report) with
              | [], None ->
                  "No CI minimization requests made?"
              | [], Some msg ->
                  "I was unable to minimize any of the CI targets that you \
                   requested." ^ try_again_msg ^ "\n" ^ msg
              | _ :: _, _ ->
                  (* TODO: change https://github.com/rocq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
                  f
                    "I am now [%s \
                     minimization](https://github.com/rocq-community/run-coq-bug-minimizer/actions) \
                     at commit %s on requested %s %s. I'll come back to you \
                     with the results once it's done.%s\n\n\
                     %s"
                    (if Option.is_none bug_file then "running" else "resuming")
                    head
                    (pluralize "target" successful_requests)
                    (successful_requests |> String.concat ~sep:", ")
                    note_some_base_unfinished_msg
                    (Option.value ~default:"" unsuccessful_requests_report) )
              |> Lwt.return_some
          | RequestSuggested, [], None ->
              ( match possible_jobs_to_minimize with
              | [] ->
                  f "No CI jobs are available to be minimized for commit %s.%s"
                    head try_again_msg
              | _ :: _ ->
                  f
                    "You requested minimization of suggested failing CI jobs, \
                     but no jobs were suggested at commit %s. You can trigger \
                     minimization of %s with `ci minimize all` or by \
                     requesting some targets by name.%s"
                    head
                    ( possible_jobs_to_minimize
                    |> List.map ~f:(fun (_, {target}) -> target)
                    |> String.concat ~sep:", " )
                    may_wish_to_wait_msg )
              |> Lwt.return_some
          | RequestSuggested, [], Some failed_minimization_description ->
              f
                "I attempted to minimize suggested failing CI jobs at commit \
                 %s, but was unable to succeed on any jobs.%s\n\
                 %s"
                head try_again_msg failed_minimization_description
              |> Lwt.return_some
          | RequestSuggested, _ :: _, _ ->
              (* TODO: change https://github.com/rocq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
              f
                "I have [initiated \
                 minimization](https://github.com/rocq-community/run-coq-bug-minimizer/actions) \
                 at commit %s for the suggested %s %s as requested.%s\n\n\
                 %s"
                head
                (pluralize "target" jobs_minimized)
                (jobs_minimized |> String.concat ~sep:", ")
                try_again_msg
                (Option.value ~default:"" failed_minimization_description)
              |> Lwt.return_some
          | Auto, jobs_minimized, failed_minimization_description -> (
              ( match
                  bad_and_unminimizable_jobs_description ~f:(fun _ -> true)
                with
              | Some msg ->
                  Lwt_io.printlf
                    "When attempting to run CI Minimization by auto on \
                     %s/%s@%s for PR #%d:\n\
                     %s"
                    owner repo head pr_number msg
              | None ->
                  Lwt.return_unit )
              >>= fun () ->
              let suggest_jobs =
                match suggested_jobs_to_minimize with
                | [] ->
                    None
                | _ ->
                    Some
                      (f
                         ":runner: <code>@%s ci minimize</code> will minimize \
                          the following %s: %s"
                         bot_info.github_name
                         (pluralize "target" suggested_jobs_to_minimize)
                         ( suggested_jobs_to_minimize
                         |> List.map ~f:(fun {target} -> target)
                         |> String.concat ~sep:", " ) )
              in
              let suggest_only_all_jobs =
                let pre_message =
                  f
                    "- If you tag me saying `@%s ci minimize all`, I will \
                     additionally minimize the following %s (which I do not \
                     suggest minimizing):"
                    bot_info.github_name
                    (pluralize "target" possible_jobs_to_minimize)
                in
                match possible_jobs_to_minimize with
                | [] ->
                    None
                | [(reason, {target})] ->
                    Some
                      (f "%s %s (because %s)\n\n\n" pre_message target reason)
                | _ ->
                    Some
                      (f "%s\n%s\n\n\n" pre_message
                         ( possible_jobs_to_minimize
                         |> List.map ~f:(fun (reason, {target}) ->
                                f "  - %s (because %s)" target reason )
                         |> String.concat ~sep:"\n" ) )
              in
              match
                ( jobs_minimized
                , failed_minimization_description
                , suggest_jobs
                , suggest_only_all_jobs
                , suggest_minimization )
              with
              | [], None, None, None, _ ->
                  Lwt_io.printlf
                    "No candidates found for minimization on %s/%s@%s for PR \
                     #%d."
                    owner repo head pr_number
                  >>= fun () -> Lwt.return_none
              | [], None, None, Some msg, _ ->
                  Lwt_io.printlf
                    "No suggested candidates found for minimization on \
                     %s/%s@%s for PR #%d:\n\
                     %s"
                    owner repo head pr_number msg
                  >>= fun () -> Lwt.return_none
              | [], None, Some suggestion_msg, _, Error reason ->
                  Lwt_io.printlf
                    "Candidates found for minimization on %s/%s@%s for PR #%d, \
                     but I am not commenting because minimization is not \
                     suggested because %s:\n\
                     %s\n\
                     %s"
                    owner repo head pr_number reason suggestion_msg
                    (Option.value ~default:"" suggest_only_all_jobs)
                  >>= fun () -> Lwt.return_none
              | [], Some failed_minimization_description, _, _, _ ->
                  Lwt_io.printlf
                    "Candidates found for auto minimization on %s/%s@%s for PR \
                     #%d, but all attempts to trigger minimization failed:\n\
                     %s"
                    owner repo head pr_number failed_minimization_description
                  >>= fun () -> Lwt.return_none
              | [], None, Some suggestion_msg, _, Ok () ->
                  f
                    ":red_circle: CI %s at commit %s without any failure in \
                     the test-suite\n\n\
                     :heavy_check_mark: Corresponding %s for the base commit \
                     %s succeeded\n\n\
                     :grey_question: Ask me to try to extract %s that can be \
                     added to the test-suite\n\n\
                     <details><summary>%s</summary>\n\n\
                     - You can also pass me a specific list of targets to \
                     minimize as arguments.\n\
                     %s\n\
                     </details>%s"
                    (pluralize "failure" suggested_jobs_to_minimize)
                    head
                    (pluralize "job" suggested_jobs_to_minimize)
                    base
                    (pluralize "a minimal test case"
                       ~plural:"minimal test cases" suggested_jobs_to_minimize )
                    suggestion_msg
                    (Option.value ~default:"" suggest_only_all_jobs)
                    may_wish_to_wait_msg
                  |> Lwt.return_some
              | _ :: _, _, _, _, _ ->
                  f
                    ":red_circle: CI %s at commit %s without any failure in \
                     the test-suite\n\n\
                     :heavy_check_mark: Corresponding %s for the base commit \
                     %s succeeded\n\n\
                     <details><summary>:runner: I have automatically started \
                     minimization for %s to augment the test-suite</summary>\n\n\
                     - You can also pass me a specific list of targets to \
                     minimize as arguments.\n\
                     %s\n\
                     </details>"
                    (pluralize "failure" jobs_minimized)
                    head
                    (pluralize "job" jobs_minimized)
                    base
                    (jobs_minimized |> String.concat ~sep:", ")
                    (Option.value ~default:"" suggest_only_all_jobs)
                  |> Lwt.return_some ) )
          >>= function
          | Some message ->
              GitHub_mutations.post_comment ~id:comment_thread_id ~message
                ~bot_info
              >>= GitHub_mutations.report_on_posting_comment
          | None ->
              Lwt_io.printlf
                "NOT commenting with CI minimization information at %s/%s@%s \
                 (PR #%d)."
                owner repo head pr_number )
      | Error err ->
          let message = run_ci_minimization_error_to_string err in
          if comment_on_error then
            GitHub_mutations.post_comment ~id:comment_thread_id ~message
              ~bot_info
            >>= GitHub_mutations.report_on_posting_comment
          else
            Lwt_io.printlf "Error while attempting to minimize from PR #%d:\n%s"
              pr_number message )
  | Error (Some comment_thread_id, err) when comment_on_error ->
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f "Error while attempting to find job minimization information:\n%s"
             err )
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment
  | Error (_, err) ->
      Lwt_io.printlf
        "Error while attempting to find jobs to minimize from PR #%d:\n%s"
        pr_number err
