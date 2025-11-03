open Base

let f = Printf.sprintf

(* GitHub specific *)

let project_api_preview_header =
  [("Accept", "application/vnd.github.inertia-preview+json")]

let app_api_preview_header =
  [("Accept", "application/vnd.github.machine-man-preview+json")]

let api_json_header = [("Accept", "application/vnd.github+json")]

let toml_of_string s = Toml.Parser.(from_string s |> unsafe)

let subkey_value toml_table k k' =
  Toml.Lenses.(get toml_table (key k |-- table |-- key k' |-- string))
