(** Reads from the source, logs results, and returns either the value or an error.
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

(** {1 Basic types} *)

(** [string ?secret name] reads [name] from a source and returns its value as a string. *)
val string : ?secret:bool -> string -> string t

(** [int ?secret name] reads the value of [name] from a source and interprets it as an int.

    {@ocaml[
      Oenv.(int "PORT" |> read) (* when PORT=8080 *) = Ok 8080
      Oenv.(int "PORT" |> read) (* when PORT=8_080 *) = Ok 8080
    ]}
    *)
val int : ?secret:bool -> string -> int t

(** [bool ?secret name] reads [name] from a source and interprets the value as a bool. Only ["true"] or ["false"] are accepted. *)
val bool : ?secret:bool -> string -> bool t

(** {1 Optional} *)

(** {!val-optional} allows missing values and returns [None] instead of an error.

    {@ocaml[
    (* when USER is unset *) 
    Oenv.(optional (string "USER") |> read) = Ok None
    ]}
    *)
val optional : 'a t -> 'a option t

(** Sets default value to {!val-optional} reader.

    {@ocaml[
    (* when USER is unset *)
    Oenv.(optional (string "USER") |> default "guest" |> read) = Ok "guest"
    ]}
    *)
val default : 'a -> 'a option t -> 'a t

(** {1 Custom types}

    Oenv allows custom types with a validator.
    *)

(** Creates a new custom reader. *)
val custom : ?secret:bool -> string -> (string -> ('a, Errors.t) Result.t) -> 'a t

(** Reads [name] as a list separated by [sep] (default is [',']), validating each element with [validate].

    {@ocaml[
    (* when HOSTS="a,b,c" *) 
    Oenv.(list "HOSTS" Result.ok |> read) = Ok ["a"; "b"; "c"]
    ]}
    *)
val list
  :  ?secret:bool
  -> ?sep:char
  -> string
  -> (string -> ('a, Errors.t) Result.t)
  -> 'a list t
