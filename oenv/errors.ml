type t =
  [ `Missing of string
  | `Nested_optional of string
  | `Parse of string * string
  | `Exn of exn
  ]
