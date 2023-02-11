type t = 
  | Int
  | Bool
  [@@deriving equal, compare, sexp]

let _tostring = function 
  | Int -> "int"
  | Bool -> "bool"