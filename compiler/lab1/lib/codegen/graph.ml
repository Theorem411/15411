open Core
(*_ 
    * Graph type definition
 *)

module AS = Assem
module Vertex = struct 
  (* type reg = EAX | EDX [@@deriving compare, sexp]  *)
  type reg = AS.reg [@@deriving compare, sexp]
  (*_ OH : why could I use Assem.reg ? *)
  module T = struct 
    type t = R of AS.reg | T of Temp.t [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make (T)
  (*_ let reg_enum = function 
    AS.EAX -> 0
  | AS.EBX -> 1
  | AS.ECX -> 2
  | AS.EDX -> 3 
  | AS.ESI -> 4
  | AS.EDI -> 5
  | AS.R8D -> 6 
  | AS.R9D -> 7
  | AS.R10D -> 8
  | AS.R12D -> 9
  | AS.R13D -> 10
  | AS.R14D -> 11
  | AS.R15D -> 12 *)
  let op_to_vertex_opt = function
    AS.Reg r -> Some (R r) 
  | AS.Temp t -> Some (T t) 
  | _ -> None
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
type color_palette_t = (int option) Vertex.Map.t [@@deriving sexp]
(*_ 
    precolor: give registers different colors 
  *)
let precolor (graph : t) : color_palette_t = 
  let vs = Vertex.Map.key_set graph in
  let fold_f acc v = Vertex.Map.update acc v ~f:(fun _ -> match v with Vertex.R r -> Some (AS.reg_enum r) | _ -> None) in 
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
      let weights' = Vertex.Map.remove weights v_max in
      let weights'' = Vertex.Map.mapi weights' ~f:incr_fn in (*_ new changes *)
        v_max :: (aux wkset' weights'' (i-1))
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

 type debug_2 = (Vertex.t * Vertex.t) list  [@@deriving sexp]
 
 type debug_3 = Vertex.t list  [@@deriving sexp]
 
 
let coloring (graph : t) : (Vertex.t * int) list = 
  let vertex_order = ordering graph in
  let () = CustomDebug.print_with_name "\n vertex order" [sexp_of_debug_3 vertex_order] in
  let color_palette = precolor graph in 
  let () = CustomDebug.print_with_name "\n color palette" [sexp_of_color_palette_t color_palette] in
  coloring_aux graph color_palette vertex_order
;;

(*_ Liveness
    *)
type live_in_out_t = Vertex.Set.t * Vertex.Set.t [@@deriving sexp]
(*_ init live in & out: [{EAX}, {}]*)
let live_in_out_init : live_in_out_t = (Vertex.Set.singleton (Vertex.R AS.EAX), Vertex.Set.empty)
;;


(* let is_use_1 (op:AS.operand) : Vertex.Set.t = 
  match op with 
    AS.Reg r -> Vertex.Set.singleton (Vertex.R r)
  | AS.Temp t -> Vertex.Set.singleton (Vertex.T t)
  | _ -> Vertex.Set.empty
;;
let is_use_2 (op1, op2) =  Vertex.Set.union (is_use_1 op1) (is_use_1 op2)
;; *)
let is_use (op_lst : AS.operand list) = 
  List.filter ~f:(fun op -> match op with AS.Reg _ -> true | AS.Temp _ -> true | _ -> false) op_lst 
  |> List.filter_map ~f:Vertex.op_to_vertex_opt
  |> Vertex.Set.of_list
let uses (l : AS.instr) = 
  match l with 
    AS.Binop binop -> 
      let op_lst = (
        match binop.op with 
          AS.Div -> [AS.Reg AS.EAX; AS.Reg AS.EDX] 
        | _ -> []) in
        is_use (binop.lhs::binop.rhs::op_lst)
  | AS.Mov mv -> 
      let op_lst = [mv.src] in
        is_use (op_lst)
  | _ -> Vertex.Set.empty 
;;
let is_def (d : AS.operand) : Vertex.Set.t = (*_ result is either empty or singleton*)
  match (Vertex.op_to_vertex_opt d) with 
  None -> Vertex.Set.empty | Some v -> Vertex.Set.singleton v
;;

let defs (l : AS.instr) = 
  match l with 
    AS.Binop binop -> is_def binop.dest
  | AS.Mov mv -> is_def mv.dest
  | _ -> Vertex.Set.empty
;;
let not_live = function
    AS.Binop binop -> (
      match binop.dest with 
        AS.Reg r -> Vertex.Set.singleton (Vertex.R r)
      | AS.Temp t -> Vertex.Set.singleton (Vertex.T t)
      | _ -> Vertex.Set.empty)
  | AS.Mov mv -> 
      List.filter_map [mv.dest; mv.src] ~f:Vertex.op_to_vertex_opt |> Vertex.Set.of_list
  | _ -> Vertex.Set.empty
;;
let live_in_out_aux (l : AS.instr) in_out = 
  match in_out with 
      [] -> [] (*_ should not get here *)
    | (_, (_, live_info))::_ -> 
        let (live_in, live_out) = live_info in
        let live_in' = (Vertex.Set.diff live_out (defs l)) |> Vertex.Set.union (uses l) in 
        let live_out' = live_in in (*_ will modify when branching *)
        let live_set = Vertex.Set.diff live_out' (not_live l) in
        (match l with 
          AS.Binop binop -> 
            let v_cur = Vertex.op_to_vertex_opt (binop.dest) in
              (v_cur, (live_set, (live_in', live_out')))::in_out
        | AS.Mov mv -> 
            let v_cur = Vertex.op_to_vertex_opt (mv.dest) in
              (v_cur, (live_set, (live_in', live_out')))::in_out
        | _ -> in_out
        )
      
;;
let live_in_out (prog : AS.instr list) = 
  List.fold_right prog ~f:live_in_out_aux ~init:[(None, (Vertex.Set.empty, live_in_out_init))]
;;


type debug_1 = (Vertex.t option * (Vertex.Set.t * live_in_out_t)) list [@@deriving sexp]

(*_ Live ranges: an alternative way to create interference graph (banished)
    *)

(*_ mk interference graph using livenes
    *)
let mk_interfere_graph (prog : AS.instr list) = 
  let live_info = live_in_out prog in 
  let map_f (vopt, (live_after, _)) = (
    match vopt with 
        None -> None
      | Some v -> Some (v, live_after)
  ) in
  let live_edge = List.map live_info ~f:map_f in
  let fold_f acc inter_info = (
    match inter_info with 
      None -> acc
    | Some (v, inter_set) -> 
        let new_edges = Vertex.Set.to_list inter_set |> List.map ~f:(fun u -> (v, u)) in
          new_edges :: acc
  ) in
  let edge_list_list = List.fold live_edge ~init:[] ~f:fold_f in
  let edge_list = List.concat edge_list_list in
  let () = CustomDebug.print_with_name "\nedge_list" [sexp_of_debug_2 edge_list] in
    from_list edge_list
