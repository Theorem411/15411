open Core

type t =
  | Int
  | Bool
  | Star of t
  | Array of t
  | Struct of Symbol.t
  | Any (*_ only for Null pointer only *)
[@@deriving equal, compare, sexp]

type tau =
  | RealTyp of t
  | FakeTyp of Symbol.t
  | Star of tau
  | Array of tau
  | Struct of Symbol.t
[@@deriving equal, compare, sexp]

type fsig_real = t list * t option [@@deriving equal, compare, sexp]
type fsig = tau list * tau option [@@deriving equal, compare, sexp]
type ssig_real = (Symbol.t * t) list [@@deriving equal, compare, sexp]
type ssig = (Symbol.t * tau) list [@@deriving equal, compare, sexp]

module T = struct
  type t = tau [@@deriving compare, equal, sexp]
end

include Comparable.Make (T)

(*_ format types *)
let rec _t_tostring = function
  | Int -> "int"
  | Bool -> "bool"
  | Star t -> sprintf "%s*" (_t_tostring t)
  | Array t -> sprintf "%s[]" (_t_tostring t)
  | Struct s -> sprintf "struct %s" (Symbol.name s)
  | Any -> "any"
;;

let rec _tau_tostring = function
  | RealTyp t -> _t_tostring t
  | FakeTyp s -> Symbol.name s
  | Star tau -> sprintf "pointer type: (%s*)" (_tau_tostring tau)
  | Array tau -> sprintf "%s[]" (_tau_tostring tau)
  | Struct s -> sprintf "struct %s" (Symbol.name s)
;;

let _ssig_real_tostring (ssig : ssig_real) =
  List.map ssig ~f:(fun (s, t) -> sprintf "%s:%s" (Symbol.name s) (_t_tostring t))
  |> String.concat ~sep:", "
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
