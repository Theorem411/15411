open Core

type t =
  | Int
  | Bool
[@@deriving equal, compare, sexp, hash]

type tau =
  | RealTyp of t
  | FakeTyp of Symbol.t
[@@deriving equal, compare, sexp, hash]

type fsig_real = t list * t option [@@deriving equal, compare, sexp]
type fsig = tau list * tau option [@@deriving equal, compare, sexp]

module T = struct
  type t = tau [@@deriving compare, equal, sexp]
end

include Comparable.Make (T)

let _t_tostring = function
  | Int -> "int"
  | Bool -> "bool"
;;

let _tau_tostring = function
  | RealTyp t -> _t_tostring t
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

let _fsig_real_tostring ((argtyps, ret) : fsig_real) : string =
  let rettyp =
    match ret with
    | None -> "void"
    | Some t -> _t_tostring t
  in
  let argsstr t = Printf.sprintf "%s, " (_t_tostring t) in
  let argstyp = List.map argtyps ~f:argsstr |> String.concat in
  "(" ^ argstyp ^ ") -> " ^ rettyp
;;
