open Core

type color = int [@@deriving compare, sexp_of, hash]
type vertex = int [@@deriving compare, sexp_of, hash]
type t = int Array.t Array.t (* adjacency list *)

(* this ordering function implements maximum cardinality search, which guarantees
   to find a simplicial elimination ordering when run on chordal graph \
   *)

let ordering (graph : t) : vertex list = 
  let n = Array.length graph in
  let weights : (vertex, int, Int.comparator_witness) Map.t = Map.of_alist_exn (module Int) (List.init n ~f:(fun (i:int) -> (i, 0))) in 
  let vertexSet = Set.of_list (module Int) (List.init n ~f:(fun i -> i)) in 
  let rec aux (vertexSet : (vertex, Int.comparator_witness) Set.t) (weights: (vertex, int, Int.comparator_witness) Map.t) = function 
    0 -> []
  | i -> 
      let (v_max, _) = Map.max_elt_exn weights in 
      let neighbors_of_v = graph.(v_max) |> Set.of_array (module Int) in 
      let intersect = Set.inter neighbors_of_v vertexSet in 
      let incr_fn ~key ~data = if Set.mem intersect key then data+1 else data in 
      let weights' = Map.mapi weights ~f:incr_fn in
      v_max :: (aux (Set.remove vertexSet v_max) weights' (i-1))
  in
    aux vertexSet weights n
;;
(*
(* a greed graph coloring algorithm *)
type color_map = (color option) Array.t *)

(* BUG: find the smallest color that is not used in the neighborhodd*)
(* let least_color_in_neighbor  *)
(* let rec coloring_aux (graph : t) (cur_color : color_map) = function
  [] -> []
| v::vs -> (
  let f x = match cur_color.(x) with None -> false | Some _ -> true in
  let nbrs_colored = Array.filter ~f:f (graph.(v)) in
    match Array.min_elt nbrs_colored ~compare:Int.compare with 
        None -> cur_color.(v) <- (Some 0); ((v, 0) :: coloring_aux graph cur_color vs)
      | Some c -> cur_color.(v) <- (Some (c+1)); ((v, c+1) :: coloring_aux graph cur_color vs)
  ) 
;; *)

(* let coloring (graph : t) : (vertex * color) list = 
  let n = Array.length graph in
  let vertex_order = ordering graph in
  coloring_aux graph (Array.init n ~f:(fun v -> None)) vertex_order
;; *)

let coloring (_ : t) : (vertex * color) list = raise (Failure "Not Implemented");;

let to_list: t -> (vertex * vertex list) list = raise (Failure "Not Implemented");;