open Core
module T = struct 
  type reg = EAX | EDX [@@deriving compare, sexp] (** OH : why could I use Assem.reg ? *)
  type t = R of reg | T of Temp.t
  [@@deriving compare, sexp]
end
include T
include Comparable.Make (T)