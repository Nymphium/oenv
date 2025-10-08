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
type ('func, 'final) builder

(** [v] creates new builder.

    {@ocaml[
    (* Validates HOST and PORT then creates a tuple. *)
    Oenv.Product.(v (fun l r -> (l, r))
      +: string "HOST"
      +: int "PORT"
      |> close)
    ]}
    *)
val v : 'func -> ('func, 'final) builder

(** [+:] appends a reader. *)
val ( +: ) : ('a -> 'b, 'final) builder -> 'a t -> ('b, 'final) builder

(** [close]s the builder and creates a reader. *)
val close : ('final, 'final) builder -> 'final t
