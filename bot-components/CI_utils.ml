open Base

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
