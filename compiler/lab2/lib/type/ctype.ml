type t = 
  | Int
  | Bool
  [@@deriving compare, sexp]