open Core

type t =
  | Int
  | Bool
[@@deriving equal, compare, sexp, hash]

type tau =
  | RealTyp of t
  | FakeTyp of Symbol.t
[@@deriving equal, compare, sexp, hash]

module T = struct
  type t = tau
  [@@deriving compare, equal, sexp]
end
include Comparable.Make(T)

type fsig = tau list * tau option
type fsig_real = t list * t option

let _tau_tostring = function
  | RealTyp t ->
    (match t with
     | Int -> "int"
     | Bool -> "bool")
  | FakeTyp s -> Symbol.name s
;;

let _fsig_tostring ((argtyps, ret) : fsig) : string =
  let rettyp =
    match ret with
    | None -> "void"
    | Some t -> _tau_tostring t
  in
  let argsstr tau = Printf.sprintf "%s, " (_tau_tostring tau) in
  let argstyp = List.map argtyps ~f:argsstr |> String.concat in
  "(" ^ argstyp ^ ") -> " ^ rettyp
;;
