open Core
type reg = EAX | EDX [@@deriving compare, sexp] (*_ OH : why could I use Assem.reg ? *)
type t = R of reg | T of Temp.t [@@deriving compare]
include Comparable.S with type t := t