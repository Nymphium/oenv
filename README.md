Oenv
===

A composeable environment variable reader for OCaml

# Example

```ocaml
let ( let* ) m k = Result.bind in
let* debug = Oenv.(bool "DEBUG" |> read) in
let* api_key = Oenv.(string "API_KEY" |> read) in
Printf.printf "settigns: debug: %b, api_key: %s\n" debug api_key;
Result.ok ()
```

# Features
- Basic types: string, int, bool
- 2nd types: list, option
- Record
- Custom types

See [./examples](examples/)
