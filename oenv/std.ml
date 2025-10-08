(** {1 Readers}

    Reads from the source, logs results, and returns either the value or an error.
    Each reader takes the [name] of a source and an optional [secret] flag.
    If a reader cannot find [name], it returns [`Missing name].
    {{!val-optional} [optional]} allows missing values and returns [None].

    If [secret] is [true] (by default), the value is redacted in error messages.

    {@ocaml[
    let* api_key = Oenv.(string "API_KEY" |> read) in
    ...
    (* main.exe: [INFO] Found API_KEY: <secret> *)
    ]}

    See {!Logging} to configure logging.
    *)

open Shapes

open struct
  let create ?(secret = true) shape name validate =
    conceal @@ field { shape = Shape shape; name; validate; secret }
  ;;
end

(** {2 Basic types} *)

(** [string ?secret name] reads [name] from a source and returns its value as a string. *)
let string ?secret name = create ?secret String name Result.ok

(** [int ?secret name] reads the value of [name] from a source and interprets it as an int.

    {@ocaml[
      Oenv.(int "PORT" |> read) (* when PORT=8080 *) = Ok 8080
      Oenv.(int "PORT" |> read) (* when PORT=8_080 *) = Ok 8080
    ]}
    *)
let int ?secret name =
  let validate s =
    try Ok (int_of_string s) with
    | Failure _ -> Error (`Parse ("int", s))
  in
  create ?secret Int name validate
;;

(** [bool ?secret name] reads [name] from a source and interprets the value as a bool. Only ["true"] or ["false"] are accepted. *)
let bool ?secret name =
  create ?secret Bool name @@ function
  | "true" -> Ok true
  | "false" -> Ok false
  | v -> Error (`Parse ("bool", v))
;;

(** {2 Optional} *)

(** {!val-optional} allows missing values and returns [None] instead of an error.

    {@ocaml[
    (* when USER is unset *) 
    Oenv.(optional (string "USER") |> read) = Ok None
    ]}
    *)
let optional s =
  map s @@ function
  | Ok v -> Ok (Some v)
  | Error (`Missing _) -> Ok None
  | Error e -> Error e
;;

(** Sets default value to {!val-optional} reader.

    {@ocaml[
    (* when USER is unset *)
    Oenv.(optional (string "USER") |> default "guest" |> read) = Ok "guest"
    ]}
    *)
let default value opt =
  map opt @@ function
  | Ok (Some v) -> Ok v
  | Ok None | Error (`Missing _) -> Ok value
  | Error e -> Error e
;;

(** {2 Custom types}

    Oenv allows custom types with a validator.
    *)

(** Creates a new custom reader. *)
let custom ?secret name validate = create ?secret Custom name validate

(** Reads [name] as a list separated by [sep] (default is [',']), validating each element with [validate].

    {@ocaml[
    (* when HOSTS="a,b,c" *) 
    Oenv.(list "HOSTS" Result.ok |> read) = Ok ["a"; "b"; "c"]
    ]}
    *)
let list ?secret ?(sep = ',') name validate =
  create ?secret Custom name @@ fun s ->
  if String.length s = 0
  then Ok []
  else (
    let items = String.split_on_char sep s in
    let[@tmc] rec aux acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs -> validate x |> Fun.flip Result.bind @@ fun v -> aux (v :: acc) xs
    in
    aux [] items)
;;
