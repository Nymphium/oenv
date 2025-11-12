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

type ('a, 'err) schema = (string -> string option) -> ('a, 'err) result

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

let[@inline] conceal s = S (s, fun err -> (err :> [> Errors.t ]))

let[@inline] value s =
  let (S (scm, reify)) = s in
  fun source -> scm source |> Result.map_error reify
;;

let[@inline] map s f = conceal @@ Fun.compose f @@ value s

let widen r =
  Fun.flip Result.map_error r @@ function
  | #Errors.t as err -> err
;;

module Export : sig
  type nonrec 'a t = 'a t

  val read_source : 'a t -> (string -> string option) -> ('a, [> Errors.t ]) Result.t
  val read : 'a t -> ('a, [> Errors.t ]) Result.t

  (** {!read_source} reads from a source and returns its result.
    {!read} is [read_source Sys.getenv_opt].
    *)
end = struct
  type nonrec 'a t = 'a t

  let read_source s map =
    value s map
    |> Result.map_error @@ function
       | #Errors.t as err -> err
  ;;

  let read s = read_source s Sys.getenv_opt
end
