type 'a t
type getter = string -> string option
type 'a validator = string -> ('a, Errors.t) Result.t

val string : string -> string t
val int : string -> int t
val bool : string -> bool t
val custom : string -> 'a validator -> 'a t
val list : ?sep:char -> string -> 'a validator -> 'a list t
val optional : 'a t -> 'a option t
val default : 'a -> 'a option t -> 'a t

module Record : sig
  type ('func, 'final) builder

  val v : 'func -> ('func, 'final) builder
  val ( +: ) : ('a -> 'b, 'final) builder -> 'a t -> ('b, 'final) builder
  val close : ('final, 'final) builder -> 'final t
end

val read_source : 'a t -> getter -> ('a, Errors.t) result
val read : 'a t -> ('a, Errors.t) result
