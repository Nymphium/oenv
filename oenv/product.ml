(** Product builder combinators

    {@ocaml[
    type config =
      { host : string
      ; int : port
      ; user : string option
      }

    let make host port user = { host; port; user }

    let env = Product.(
      v make
      +: string var_host
      +: int var_port
      +: optional (string var_user))
    ]}

    *)

open Shapes

(** An {i opened} product builder. {{!close} [close]} to close the builder and get {{!t} ['a t]}. *)
type ('func, 'final) builder = 'func t

(** [v] creates new builder.

    {@ocaml[
    (* Validates HOST and PORT then creates a tuple. *)
    Oenv.Product.(v (fun l r -> (l, r))
      +: string "HOST"
      +: int "PORT"
      |> close)
    ]}
    *)
let v x : ('func, 'final) builder = conceal @@ Fun.const @@ Ok x

open struct
  let apply pf px =
    fun getter ->
    match pf getter with
    | Error e -> Error e
    | Ok f ->
      (match px getter with
       | Error e -> Error e
       | Ok x -> Ok (f x))
  ;;
end

(** [+:] appends a reader. *)
let ( +: ) (b : ('a -> 'b, 'final) builder) (s : 'a t) : ('b, 'final) builder =
  let b =
    let (S (b, upcast)) = b in
    fun getter ->
      match b getter with
      | Error e -> Error (upcast e)
      | Ok v -> Ok v
  in
  let s =
    let (S (s, upcast')) = s in
    fun getter ->
      match s getter with
      | Error e -> Error (upcast' e)
      | Ok v -> Ok v
  in
  conceal @@ apply b s
;;

(** [close]s the builder and creates a reader. *)
let close : ('final, 'final) builder -> 'final t = Fun.id
