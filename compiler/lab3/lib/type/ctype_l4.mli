open Core

type t =
  | Int
  | Bool
  | Star of t
  | Array of t
  | Struct of
      { sname : Symbol.t
      ; ssig : (Symbol.t * t) list
      }
  | Any (*_ only for Null pointer only *)
[@@deriving equal, compare, sexp]

type tau =
  | RealTyp of t
  | FakeTyp of Symbol.t
  | Star of tau
  | Array of tau
  | Struct of
      { sname : Symbol.t
      ; ssig : (Symbol.t * tau) list
      }
[@@deriving equal, compare, sexp]

type fsig = tau list * tau option [@@deriving equal, compare, sexp]
type fsig_real = t list * t option [@@deriving equal, compare, sexp]

include Comparable.S with type t := tau

val _t_tostring : t -> string
val _tau_tostring : tau -> string
val _fsig_tostring : fsig -> string
val _fsig_real_tostring : fsig_real -> string
