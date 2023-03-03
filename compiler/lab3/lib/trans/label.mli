open Core

type t [@@deriving compare, sexp, hash]

include Comparable.S with type t := t

(* resets label numbering *)
val reset : unit -> unit

(* returns a unique new label *)
val create : unit -> t

(* returns the name of a label *)
val name : t -> string
