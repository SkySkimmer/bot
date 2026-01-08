open Base
open Test_helpers

let test_coq_bug_minimizer_requires_pat () =
  let bot_info = create_bot_info () in
  match
    Result.try_with (fun () ->
        Coq.git_coq_bug_minimizer ~bot_info ~script:"script"
          ~comment_thread_id:(Bot_components.GitHub_ID.of_string "1")
          ~comment_author:"author" ~owner:"owner" ~repo:"repo" ~coq_version:"v"
          ~ocaml_version:"v" ~minimizer_extra_arguments:[] )
  with
  | Ok _ ->
      Alcotest.fail "Expected git_coq_bug_minimizer to fail without PAT"
  | Error _ ->
      ()

let test_run_ci_minimization_requires_pat () =
  let bot_info = create_bot_info () in
  match
    Result.try_with (fun () ->
        Coq.git_run_ci_minimization ~bot_info
          ~comment_thread_id:(Bot_components.GitHub_ID.of_string "1")
          ~owner:"owner" ~repo:"repo" ~pr_number:"1" ~docker_image:"img"
          ~target:"target" ~ci_targets:[] ~opam_switch:"sw" ~failing_urls:""
          ~passing_urls:"" ~base:"base" ~head:"head"
          ~minimizer_extra_arguments:[] ~bug_file_name:None )
  with
  | Ok _ ->
      Alcotest.fail "Expected git_run_ci_minimization to fail without PAT"
  | Error _ ->
      ()

let () =
  Alcotest.run "PAT usage tests"
    [ ( "requires PAT"
      , [ ( "coq_bug_minimizer requires PAT"
          , `Quick
          , test_coq_bug_minimizer_requires_pat )
        ; ( "run_ci_minimization requires PAT"
          , `Quick
          , test_run_ci_minimization_requires_pat ) ] ) ]
