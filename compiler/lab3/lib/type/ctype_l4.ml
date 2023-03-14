open Core

type t =
  | Int
  | Bool
  | Star of t
  | Array of t
  | Struct of
      { sname : Symbol.t
      ; ssig : (Symbol.t * t) list
      }
  | Any (*_ only for Null pointer only *)
[@@deriving equal, compare, sexp]

type tau =
  | RealTyp of t
  | FakeTyp of Symbol.t
  | Star of tau
  | Array of tau
  | Struct of
      { sname : Symbol.t
      ; ssig : (Symbol.t * tau) list
      }
[@@deriving equal, compare, sexp]

type fsig_real = t list * t option [@@deriving equal, compare, sexp]
type fsig = tau list * tau option [@@deriving equal, compare, sexp]

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
  | Struct { sname; ssig } ->
    sprintf
      "struct %s {%s}"
      (Symbol.name sname)
      (List.map ssig ~f:(fun (f, t) -> sprintf "%s:%s\n" (Symbol.name f) (_t_tostring t))
      |> String.concat ~sep:", ")
  | Any -> "any"
;;

let rec _tau_tostring = function
  | RealTyp t -> _t_tostring t
  | FakeTyp s -> Symbol.name s
  | Star tau -> sprintf "%s*" (_tau_tostring tau)
  | Array tau -> sprintf "%s[]" (_tau_tostring tau)
  | Struct { sname; ssig } ->
    sprintf
      "struct %s {%s}"
      (Symbol.name sname)
      (List.map ssig ~f:(fun (f, tau) -> sprintf "%s:%s" (Symbol.name f) (_tau_tostring tau))
      |> String.concat ~sep:", ")
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
