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
- Product
- Custom types
- [Logs](https://erratique.ch/software/logs/doc/Logs/index.html) support

## Product
`Product` creates product reader.

```ocaml
type t =
  { name : string
  ; value : string option
  }
let make name value = { name; value }

let t = Oenv.(Product.v make
  +: string "NAME"
  +: string "VALUE" |> option
  |> close)

Oenv.read t
```

# Custom types
Make custom type reader with `custom` combinator.

```ocaml
type flag = A | B
let of_string = function
  | "A" -> Ok A
  | "B" -> Ok B
  | other -> Error (`Parse ("flag", other))

let flag = Oenv.custom ~secret:false "FLAG" of_string
```

---

See [./examples](examples/) and [reference](https://nymphium.github.io/oenv/oenv) for more details.
