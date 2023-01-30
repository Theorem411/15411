open Core
(*_ 
    * Graph type definition
 *)
module Vertex = struct 
  type reg = EAX | EDX [@@deriving compare, sexp] (*_ OH : why could I use Assem.reg ? *)
  module T = struct 
    type t = R of reg | T of Temp.t [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make (T)
  let reg_enum = function 
    EAX -> 0
  | EDX -> 1
end 

type t = Vertex.Set.t Vertex.Map.t (* adjacency list *)

(*_
    * Graph utility functions
*)
let graph_empty = Vertex.Map.empty
;;
let add_edge (graph:t) ((u, v): Vertex.t * Vertex.t) = 
  let graph = Vertex.Map.update graph u ~f:(fun o -> match o with None -> Vertex.Set.singleton v | Some s -> Vertex.Set.add s v) in
  let graph = Vertex.Map.update graph v ~f:(fun o -> match o with None -> Vertex.Set.singleton u | Some s -> Vertex.Set.add s u) in 
    graph
;;
let from_list (edge_list: (Vertex.t * Vertex.t) list) = 
  List.fold edge_list ~init:graph_empty ~f:add_edge
;;
let to_list (graph: t) : (Vertex.t * Vertex.t list) list = 
  let res = Vertex.Map.to_alist ~key_order:`Decreasing graph in 
  let map_f = (fun (v, vs) -> (v, Vertex.Set.to_list vs)) in
    List.map res ~f:map_f 
;;
(*_ 
    graph coloring 
  *)
type color_palette_t = (int option) Vertex.Map.t
(*_ 
    precolor: give registers different colors 
  *)
let precolor (graph : t) : color_palette_t = 
  let vs = Vertex.Map.key_set graph in
  let fold_f acc v = Vertex.Map.update acc v ~f:(fun _ -> match v with Vertex.R r -> Some (Vertex.reg_enum r) | _ -> None) in 
  Vertex.Set.fold vs ~init:(Vertex.Map.empty) ~f:fold_f
;;
(*_ 
    initial weighting 
  *)
let nbrs_weight (graph: t) (color_palette : color_palette_t) (v: Vertex.t) = 
  let nbrs = Vertex.Map.find_exn graph v in 
  let fold_f ncol v = (match (Vertex.Map.find_exn color_palette v) with Some _ -> ncol+1 | None -> ncol) in
  Vertex.Set.fold nbrs ~init:0 ~f:fold_f
;;
let initial_weight (graph : t) : int Vertex.Map.t = 
  let vs = Vertex.Map.key_set graph in
  let color_palette = precolor graph in
  let init_weights = Vertex.Map.of_key_set vs ~f:(nbrs_weight graph color_palette) in
    init_weights
;;
(*_ 
    maximum capacity searching 
  *)
let ordering (graph: t) : Vertex.t list = 
  let n = Vertex.Map.length graph in
  let weights = initial_weight graph in 
  let wkset = Vertex.Map.key_set graph in
  let rec aux wkset weights i =
    if i = 0 then []
    else 
      let (v_max, _) = Vertex.Map.max_elt_exn weights in 
      let nbrs = Vertex.Map.find_exn graph v_max in 
      let inter = Vertex.Set.inter nbrs wkset in 
      let incr_fn ~key ~data = if Vertex.Set.mem inter key then data+1 else data in 
      let wkset' = Set.remove wkset v_max in
      let weights' = Vertex.Map.mapi weights ~f:incr_fn in
        v_max :: (aux wkset' weights' (i-1))
  in  
    aux wkset weights n
(*_ 
    greedy coloring 
    *)
let unused_color_in_nbrs (graph : t) (color_palette : color_palette_t) (v : Vertex.t) = 
  let nbrs = Vertex.Map.find_exn graph v in 
  let fold_f = (fun (acc:int list) -> fun v -> match (Vertex.Map.find_exn color_palette v) with None -> acc | Some c -> c::acc) in 
  let used_colors = Vertex.Set.fold nbrs ~init:[] ~f:fold_f |> Set.of_list (module Int) in
  let rec aux i_max i = 
    if (i = i_max) then i_max 
    else if not (Set.mem used_colors i) then i else aux i_max (i+1)
  in
  match (Set.max_elt used_colors) with 
    None -> 0
  | Some c_max -> aux c_max 0
;;

let rec coloring_aux (graph : t) (color_palette : color_palette_t) = function
   [] -> []
 | v::vs -> (
   let c_new = unused_color_in_nbrs graph color_palette v in 
   let color_palette' = Vertex.Map.update color_palette v ~f:(fun _ -> Some c_new) in 
   (v, c_new) :: coloring_aux graph color_palette' vs
   ) 
 ;;
 
 let coloring (graph : t) : (Vertex.t * int) list = 
   (*_ let _ = Vertex.Map.length graph in *)
   let vertex_order = ordering graph in
   let color_palette = precolor graph in 
   coloring_aux graph color_palette vertex_order
 ;;