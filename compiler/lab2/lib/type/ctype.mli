type t = 
  | Int
  | Bool
  [@@deriving equal, compare, sexp]

val _tostring : t -> string