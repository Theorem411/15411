type t = 
  | Int
  | Bool
  | Custom of Symbol.t
  [@@deriving equal, compare, sexp]

let _tostring = function 
  | Int -> "int"
  | Bool -> "bool"
  | Custom t -> Symbol.name t