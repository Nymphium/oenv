type _ base =
  | String : string base
  | Int : int base
  | Bool : bool base
  | Custom : 'a base

(** (kinded, base) type for base *)
type (_, _) shape = Shape : 'a base -> ('a, 'a) shape

type ('k, 'a, 'err) field =
  { shape : ('k, 'a) shape
  ; validate : string -> ('a, 'err) result
  ; name : string
  ; secret : bool
  }
  constraint 'err = [> Errors.t ]

type source = string -> string option
type 'a validator = string -> ('a, Errors.t) Result.t
type ('a, 'err) schema = source -> ('a, 'err) result

(** conceal ['err] from schema with reify function *)
type _ t = S : ('a, 'err) schema * ('err -> Errors.t) -> 'a t

(* Get the value from the source and validate it. Return according to the kinded type. *)
let field (type k a) (t : (k, a, 'err) field) source : (k, 'err) result =
  let (Shape _) = t.shape in
  match source t.name with
  | None ->
    Logging.not_found t.name;
    Error (`Missing t.name)
  | Some v ->
    Logging.found t.name v t.secret;
    t.validate v
;;

let conceal s = S (s, fun err -> (err :> [> Errors.t ]))

let read_source s =
  let (S (s, upcast)) = s in
  fun source -> s source |> Result.map_error upcast
;;

let read s = read_source s Sys.getenv_opt
