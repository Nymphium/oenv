let pp_exn ppf ex = Format.fprintf ppf "%s" @@ Printexc.to_string ex
let equal_exn = ( = )

type t =
  [ `Missing of string
  | `Nested_optional of string
  | `Parse of string * string
  | `Exn of exn
  ]
[@@deriving show, eq]
