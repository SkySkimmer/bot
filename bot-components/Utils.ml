open Base

let f = Printf.sprintf

let toml_of_string s = Toml.Parser.(from_string s |> unsafe)

let subkey_value toml_table k k' =
  Toml.Lenses.(get toml_table (key k |-- table |-- key k' |-- string))
