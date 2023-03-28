open Core

type t = int [@@deriving compare, sexp, hash]

type bt =
  | BlockLbl of t
  | FunName of
      { fname : Symbol.t
      ; args : Temp.t list
      }
[@@deriving compare, equal, sexp, hash]

include Comparable.Make (Int)

let counter = ref 1
let reset () = counter := 1

let create () =
  let t = !counter in
  incr counter;
  t
;;

let name t = ".L" ^ string_of_int t

let name_bt = function 
  | BlockLbl l -> name l
  | FunName { fname; args } -> 
    sprintf ".%s(%s)"
    (Symbol.name fname)
    (List.map args ~f:(fun t -> Temp.name t) |> String.concat ~sep:", ")
  ;;
let format_args args = List.map ~f:Temp.name args |> String.concat ~sep:","

let format_bt : bt -> string = function
  | BlockLbl l -> sprintf "BlockLbl(%s):" (name l)
  | FunName { fname = f; args } ->
    sprintf "FunLbl %s(%s):" (Symbol.name f) (format_args args)
;;
