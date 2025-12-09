open Base
open Bot_components

let with_env var value_opt f =
  let restore =
    let original = Stdlib.Sys.getenv_opt var in
    fun () ->
      match original with
      | None -> (
        try Unix.putenv var "" with _ -> () )
      | Some v ->
          Unix.putenv var v
  in
  Exn.protect
    ~f:(fun () ->
      ( match value_opt with
      | None -> (
        try Unix.putenv var "" with _ -> () )
      | Some v ->
          Unix.putenv var v ) ;
      f () )
    ~finally:restore

let check_pat ~name ~toml ~env_pat ~expected =
  let toml_data = Utils.toml_of_string toml in
  with_env "GITHUB_ACCESS_TOKEN" env_pat (fun () ->
      match (Config.github_pat toml_data, expected) with
      | actual, expected when Option.equal String.equal actual expected ->
          ()
      | actual, expected ->
          Alcotest.failf "%s: expected %s, got %s" name
            (Option.value expected ~default:"None")
            (Option.value actual ~default:"None") )

let test_github_pat_from_toml () =
  check_pat ~name:"PAT from toml" ~toml:"[github]\napi_token=\"pat_toml\"\n"
    ~env_pat:None ~expected:(Some "pat_toml")

let test_github_pat_from_env () =
  check_pat ~name:"PAT from env" ~toml:"" ~env_pat:(Some "env_pat")
    ~expected:(Some "env_pat")

let test_github_pat_missing () =
  check_pat ~name:"PAT missing" ~toml:"" ~env_pat:None ~expected:None

let () =
  Alcotest.run "Config tests"
    [ ( "github_pat"
      , [ ("reads PAT from toml", `Quick, test_github_pat_from_toml)
        ; ( "reads PAT from env when toml missing"
          , `Quick
          , test_github_pat_from_env )
        ; ( "returns None when no PAT configured"
          , `Quick
          , test_github_pat_missing ) ] ) ]
