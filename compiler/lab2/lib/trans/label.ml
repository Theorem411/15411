open Core

type t = int [@@deriving compare, sexp, hash]

include Comparable.Make (Int)

let counter = ref 1
let reset () = counter := 1

let create () =
  let t = !counter in
  incr counter;
  t
;;

let name t = "%l" ^ string_of_int t ^ "."
