open Core
(*_ 
    * Graph type definition
 *)

module AS = Assem_l4

module Vertex = struct
  (* type reg = EAX | EDX [@@deriving compare, sexp]  *)
  type reg = AS.reg [@@deriving compare, sexp]

  (*_ OH : why could I use Assem.reg ? *)
  module T = struct
    type t =
      | R of AS.reg
      | T of Temp.t
    [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let op_to_vertex_opt = function
    | AS.Reg r -> Some (R r)
    | AS.Temp t -> Some (T t)
    | _ -> None
  ;;

  let _to_string = function
    | R r -> AS.format_reg r
    | T t -> Temp.name t
  ;;

  let print v = _to_string v |> print_string
end

type t = Vertex.Set.t Vertex.Map.t (* adjacency list *)

(* let print (g : t) = Vertex.Map.iter g ~f:(Vertex.Set.iter ~f:Vertex.print) *)
let print (g : t) =
  let print_aux ~key:v ~data:s =
    let s_str =
      Vertex.Set.fold s ~init:"" ~f:(fun acc u -> acc ^ Vertex._to_string u ^ ", ")
    in
    let v_str = Vertex._to_string v in
    print_string (v_str ^ ": {" ^ s_str ^ "}\n")
  in
  Vertex.Map.iteri g ~f:print_aux
;;

(*_
    * Graph utility functions
*)

let to_list (graph : t) : (Vertex.t * Vertex.t list) list =
  let res = Vertex.Map.to_alist ~key_order:`Decreasing graph in
  let map_f (v, vs) = v, Vertex.Set.to_list vs in
  List.map res ~f:map_f
;;

(* 
(*_ 
    graph coloring 
  *)
  *)
type color_palette_t = int option Vertex.Map.t [@@deriving sexp]

(*_ 
    precolor: give registers different colors 
  *)
let precolor (graph : t) : color_palette_t =
  let vs = Vertex.Map.key_set graph in
  let fold_f acc v =
    Vertex.Map.update acc v ~f:(fun _ ->
        match v with
        | Vertex.R r -> Some (AS.reg_to_enum r)
        | _ -> None)
  in
  Vertex.Set.fold vs ~init:Vertex.Map.empty ~f:fold_f
;;

(*_ 
    initial weighting 
  *)
let nbrs_weight (graph : t) (color_palette : color_palette_t) (v : Vertex.t) =
  let nbrs = Vertex.Map.find_exn graph v in
  let fold_f ncol v =
    match Vertex.Map.find_exn color_palette v with
    | Some _ -> ncol + 1
    | None -> ncol
  in
  Vertex.Set.fold nbrs ~init:0 ~f:fold_f
;;

let initial_weight (graph : t) : int Vertex.Map.t =
  let vs = Vertex.Map.key_set graph in
  let color_palette = precolor graph in
  let init_weights = Vertex.Map.of_key_set vs ~f:(nbrs_weight graph color_palette) in
  init_weights
;;

let custom_compare
    ~(get_degree : Vertex.t -> int)
    ~(temp_weight : Vertex.t -> int option)
    ((v1, w1) : Vertex.t * int)
    ((v2, w2) : Vertex.t * int)
  =
  let res = Int.compare w1 w2 in
  if res <> 0
  then res
  else (
    match temp_weight v1, temp_weight v2 with
    | Some c1, Some c2 ->
      let d1 = get_degree v1 in
      let d2 = get_degree v2 in
      Float.compare (float c1 /. float d1) (float c2 /. float d2)
    | _ -> res)
;;

(*_ 
    maximum capacity searching 
  *)
let weights_max_vertex
    ~(get_degree : Vertex.t -> int)
    ~(temp_weight : Vertex.t -> int option)
    weights
  =
  let w_list : (Vertex.t * int) list = Vertex.Map.to_alist weights in
  let res_opt = List.max_elt w_list ~compare:(custom_compare ~get_degree ~temp_weight) in
  match res_opt with
  | None -> raise (Failure "max should not be called on empty")
  | Some (vmax, _) -> vmax
;;

let ordering (graph : t) ~(temp_weight : Vertex.t -> int option) : Vertex.t list =
  let n = Vertex.Map.length graph in
  let weights = initial_weight graph in
  let wkset = Vertex.Map.key_set graph in
  let get_degree (v : Vertex.t) = Vertex.Map.find_exn graph v |> Vertex.Set.length in
  let rec aux wkset weights i =
    if i = 0
    then []
    else (
      let v_max = weights_max_vertex ~get_degree ~temp_weight weights in
      let nbrs = Vertex.Map.find_exn graph v_max in
      let inter = Vertex.Set.inter nbrs wkset in
      let incr_fn ~key ~data = if Vertex.Set.mem inter key then data + 1 else data in
      let wkset' = Set.remove wkset v_max in
      let weights' = Vertex.Map.remove weights v_max in
      let weights'' = Vertex.Map.mapi weights' ~f:incr_fn in
      v_max :: aux wkset' weights'' (i - 1))
  in
  aux wkset weights n
;;

let unused_color_in_nbrs (graph : t) (color_palette : color_palette_t) (v : Vertex.t) =
  let nbrs = Vertex.Map.find_exn graph v in
  let fold_f (acc : int list) v =
    match Vertex.Map.find_exn color_palette v with
    | None -> acc
    | Some c -> c :: acc
  in
  let used_colors = Vertex.Set.fold nbrs ~init:[] ~f:fold_f |> Set.of_list (module Int) in
  let rec aux i_max i =
    if i = i_max
    then i_max
    else if not (Set.mem used_colors i)
    then i
    else aux i_max (i + 1)
  in
  match Set.max_elt used_colors with
  | None -> 0
  | Some c_max -> aux (c_max + 1) 0
;;

let rec coloring_aux (graph : t) (color_palette : color_palette_t) = function
  | [] -> []
  | v :: vs ->
    (match Vertex.Map.find_exn color_palette v with
    | Some c_old -> (v, c_old) :: coloring_aux graph color_palette vs
    | None ->
      let c_new = unused_color_in_nbrs graph color_palette v in
      let color_palette' = Vertex.Map.update color_palette v ~f:(fun _ -> Some c_new) in
      (v, c_new) :: coloring_aux graph color_palette' vs)
;;

let coloring (graph : t) ~(temp_weight : Vertex.t -> int option) =
  let vertex_order = ordering graph ~temp_weight in
  let color_palette = precolor graph in
  let v2c = coloring_aux graph color_palette vertex_order in
  Vertex.Map.of_alist_exn v2c
;;

module VertexTable = Hashtbl.Make (Vertex)

type new_graph = Vertex.Set.t VertexTable.t

let neigh_aux g new_v (a, b) v =
  match VertexTable.find g v with
  | None -> () (* vertex not in adjacency list, do nothing *)
  | Some neighbors ->
    (* replace v1 and v2 with the new vertex in the neighbor set *)
    let new_neighbors =
      let rest =
        Vertex.Set.filter
          ~f:(fun v' -> (not (Vertex.equal v' a)) && not (Vertex.equal v' b))
          neighbors
      in
      Vertex.Set.add rest new_v
    in
    VertexTable.remove g v;
    VertexTable.add_exn g ~key:v ~data:new_neighbors
;;

(*_ Requires can_coalesce (no interferene between edges and other heuristics) g a b  *)
let coalesce (g : new_graph) ((a, b) : Vertex.t * Vertex.t) (new_v : Vertex.t) =
  (* print_endline
    (sprintf
       "before coalesing %s %s -> %s"
       (Vertex._to_string a)
       (Vertex._to_string b)
       (Vertex._to_string new_v));
  print (Vertex.Map.of_hashtbl_exn g); *)
  let a_set = VertexTable.find_exn g a in
  let b_set = VertexTable.find_exn g b in
  let u = Vertex.Set.union a_set b_set in
  VertexTable.remove g a;
  VertexTable.remove g b;
  VertexTable.add_exn g ~key:new_v ~data:u;
  Vertex.Set.iter ~f:(neigh_aux g new_v (a, b)) u;
  u
;;
