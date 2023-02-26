type t = 
  | Int
  | Bool
  [@@deriving equal, compare, sexp]

type tau = 
  | RealTyp of t
  | FakeTyp of Symbol.t
  [@@deriving equal, compare, sexp]

type fsig = tau Symbol.Map.t * tau option

val _tau_tostring : tau -> string
val _fsig_tostring : fsig -> string