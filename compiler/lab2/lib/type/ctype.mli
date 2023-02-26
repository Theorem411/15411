(* open Core *)
type t = 
  | Int
  | Bool
  [@@deriving equal, compare, sexp]

type tau = 
  | RealTyp of t
  | FakeTyp of Symbol.t
  [@@deriving equal, compare, sexp]

(* include Comparable.S with type t := Symbol.t *)
type fsig = tau Symbol.Map.t * tau option

val _tau_tostring : tau -> string
val _fsig_tostring : fsig -> string