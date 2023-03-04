open Core
type t = 
| Int
| Bool
[@@deriving equal, compare, sexp]

type tau = 
  | RealTyp of t
  | FakeTyp of Symbol.t
  [@@deriving equal, compare, sexp]

type fsig = tau list * tau option
[@@deriving equal, compare, sexp]
type fsig_real = t list * t option
[@@deriving equal, compare, sexp]

include Comparable.S with type t := tau
val _t_tostring : t -> string
val _tau_tostring : tau -> string
val _fsig_tostring : fsig -> string
