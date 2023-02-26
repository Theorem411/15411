open Core
type t = 
  | Int
  | Bool
  [@@deriving equal, compare, sexp]

type tau = 
  | RealTyp of t
  | FakeTyp of Symbol.t
  [@@deriving equal, compare, sexp]

module T = struct
  type t = Symbol.t
  [@@deriving compare, sexp]
end
include Comparable.Make(T)

type fsig = tau Symbol.Map.t * tau option

let _tau_tostring = function 
  | RealTyp t -> (match t with Int -> "int" | Bool -> "bool")
  | FakeTyp s -> Symbol.name s
;;
let _fsig_tostring (args, ret) = 
  let rettyp = (match ret with None -> "void" | Some t -> _tau_tostring t) in 
  let argsstr ~key:k ~data:d acc = acc ^ (Printf.sprintf "%s:%s, " (Symbol.name k) (_tau_tostring d)) in
  let argstyp = Map.fold args ~init:"" ~f:argsstr in 
    argstyp ^ "-> " ^ rettyp
;;