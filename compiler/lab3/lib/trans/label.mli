open Core

type t [@@deriving compare, sexp, hash]

type bt =
  | BlockLbl of t
  | FunName of
      { fname : Symbol.t
      ; args : Temp.t list
      }
[@@deriving compare, equal, sexp, hash]

include Comparable.S with type t := t

(* resets label numbering *)
val reset : unit -> unit

(* returns a unique new label *)
val create : unit -> t
val bt : t -> bt
(* returns the name of a label *)
val name : t -> string
val name_bt : bt -> string
val format_bt : bt -> string
