open Base
open GitLab_types
open Utils
open HTTP_utils
open String_utils
open CI_utils
open Lwt.Infix

module BenchResults = struct
  type t =
    { summary_table: string
    ; failures: string
    ; slow_table: string
    ; slow_number: int
    ; fast_table: string
    ; fast_number: int }
end
(******************************************************************************)
(* CI Job Info and Benchmark Utilities                                       *)
(******************************************************************************)

let fetch_bench_results ~job_info () =
  let open BenchResults in
  let open Lwt.Syntax in
  let artifact_url file =
    f
      "https://coq.gitlabpages.inria.fr/-/coq/-/jobs/%d/artifacts/_bench/timings/%s"
      job_info.build_id file
  in
  let* summary_table = artifact_url "bench_summary" |> fetch_artifact in
  let* failures =
    let* failures_or_err = artifact_url "bench_failures" |> fetch_artifact in
    match failures_or_err with
    | Ok s ->
        Lwt.return s
    | Error err ->
        Lwt_io.printlf "Error fetching bench_failures: %s" err
        >>= fun () -> Lwt.return ""
  in
  let* slow_table =
    let* slow_table_or_err = artifact_url "slow_table.html" |> fetch_artifact in
    match slow_table_or_err with
    | Ok s ->
        Lwt.return s
    | Error _ -> (
        let* slow_table_or_err = artifact_url "slow_table" |> fetch_artifact in
        match slow_table_or_err with
        | Ok s ->
            Lwt.return (String_utils.code_wrap s)
        | Error err ->
            Lwt_io.printlf "Error fetching slow_table: %s" err
            >>= fun () -> Lwt.return "" )
  in
  let* fast_table =
    let* fast_table_or_err = artifact_url "fast_table.html" |> fetch_artifact in
    match fast_table_or_err with
    | Ok s ->
        Lwt.return s
    | Error _ -> (
        let* fast_table_or_err = artifact_url "fast_table" |> fetch_artifact in
        match fast_table_or_err with
        | Ok s ->
            Lwt.return (String_utils.code_wrap s)
        | Error err ->
            Lwt_io.printlf "Error fetching fast_table: %s" err
            >>= fun () -> Lwt.return "" )
  in
  match summary_table with
  | Error e ->
      Lwt.return_error
        (f "Could not fetch table artifacts for bench summary: %s\n" e)
  | Ok summary_table -> (
      (* The tables include how many entries there are, this is useful
         information to know. *)
      let* slow_number = parse_quantity slow_table "slow" in
      let* fast_number = parse_quantity fast_table "fast" in
      match (slow_number, fast_number) with
      | Error e, _ | _, Error e ->
          Lwt.return_error (f "Fetch bench regex issue: %s" e)
      | Ok slow_number, Ok fast_number ->
          Lwt.return_ok
            { summary_table
            ; failures
            ; slow_table
            ; slow_number
            ; fast_table
            ; fast_number } )

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
