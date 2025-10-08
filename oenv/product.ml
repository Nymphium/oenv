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
    let open Result in
    fun source -> pf source |> Fun.flip bind @@ fun f -> px source |> map f
  ;;
end

(** [+:] appends a reader. *)
let ( +: ) (b : ('a -> 'b, 'final) builder) (s : 'a t) : ('b, 'final) builder =
  conceal @@ apply (value b) (value s)
;;

(** [close]s the builder and creates a reader. *)
let close : ('final, 'final) builder -> 'final t = Fun.id
