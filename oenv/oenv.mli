(** {1 Errors} *)

module Errors : sig
  (** Error type with [[@@deriving show, eq]]. *)
  type t =
    [ `Missing of string
    | `Nested_optional of string
    | `Parse of string * string
    | `Exn of exn
    ]
  [@@deriving show, eq]
end

(** {1 Core types} *)

(** An environment reader for ['a]. *)
type 'a t = 'a Shapes.t

(** A type to be read. For example, [Sys.getenv_opt]. *)
type source = string -> string option

(** A validator type. *)
type 'a validator = string -> ('a, Errors.t) Result.t

val read_source : 'a t -> source -> ('a, Errors.t) Result.t
val read : 'a t -> ('a, Errors.t) Result.t

(** {!read_source} reads from {!source} and returns its result.
    {!read} is [read_source Sys.getenv_opt].
    *)

include module type of Std

(** {1 Product builder} *)

module Product : module type of Product

(* module Product : sig *)
(*   type ('func, 'final) builder *)
(**)
(*   val v : 'func -> ('func, 'final) builder *)
(*   val ( +: ) : ('a -> 'b, 'final) builder -> 'a t -> ('b, 'final) builder *)
(*   val close : ('final, 'final) builder -> 'final t *)
(* end *)

(** {1 Logging} *)

module Logging : sig
  (** {{!Logs} [Logs]} source *)
  val src : Logs.src

  (** Sets log level of {{!Logs} [Logs]}. *)
  val set_level : Logs.level option -> unit
end
