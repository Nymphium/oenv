(** {1 Errors} *)

module Errors : module type of Errors

(** {1 Core types} *)

module Shapes : sig
  include module type of Shapes.Export
end

include module type of Shapes

(** {1 Readers} *)

include module type of Std

(** {1 Product builder} *)

module Product : module type of Product

(** {1 Logging} *)

module Logging : sig
  (** {{: https://erratique.ch/software/logs/doc/Logs/index.html} [Logs]} source *)
  val src : Logs.src

  (** Sets log level of {{: https://erratique.ch/software/logs/doc/Logs/index.html} [Logs]}. *)
  val set_level : Logs.level option -> unit
end
