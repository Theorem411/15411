type t = 
  | Int
  | Bool
  | Custom of Symbol.t
  [@@deriving equal, compare, sexp]

val _tostring : t -> string