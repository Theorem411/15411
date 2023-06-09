(* C0 Compiler
 *
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *)

open Core

module T = struct
  type t =
    { name : string [@compare.ignore] [@hash.ignore]
    ; unique_id : int
    }
  [@@deriving compare, hash, sexp]
end

include T

let cache : t String.Table.t = String.Table.create ()
let next_unique_id = ref 0

let symbol (name : string) : t =
  match Hashtbl.find cache name with
  | Some t -> t
  | None ->
    let unique_id = !next_unique_id in
    let t = { name; unique_id } in
    incr next_unique_id;
    Hashtbl.add_exn cache ~key:name ~data:t;
    t
;;

let create_fresh (_ : unit) : t =
    let unique_id = !next_unique_id in
    let name = sprintf "%s%d" "fresh_lvaluelol" unique_id in 
    let t = { name; unique_id } in
    incr next_unique_id;
    Hashtbl.add_exn cache ~key:name ~data:t;
    t
;;

let name : t -> string = fun x -> x.name

include Comparable.Make (T)

let pp_sm m ~(f : 'a -> string) =
  Map.to_alist m
  |> List.map ~f:(fun (s, v) -> sprintf "(%s:%s) " (name s) (f v))
  |> String.concat
;;

let pp_ss ss = Set.to_list ss |> List.map ~f:(fun s -> name s ^ ", ") |> String.concat
