open Core

type t = int [@@deriving compare, sexp, hash]

type bt = 
| BlockLbl of t
| FunName of {
  fname : Symbol.t
; args : Temp.t list
}

include Comparable.Make (Int)

let counter = ref 1
let reset () = counter := 1

let create () =
  let t = !counter in
  incr counter;
  t
;;

let name t = ".L" ^ string_of_int t
