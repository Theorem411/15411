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
    type t =
      | R of AS.reg
      | T of Temp.t
    [@@deriving compare, sexp]
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

(*_ 
    graph coloring 
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
      | Vertex.R r -> Some (AS.reg_enum r)
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

(*_ 
    maximum capacity searching 
  *)
(* let print_weights (weights) = 
  Vertex.Map.to_alist weights |> List.iter ~f:(fun (v, c) -> print_string (Vertex._to_string v ^","^(string_of_int c)^"\n"))
;; *)
let weights_max_vertex weights =
  let w_list : (Vertex.t * int) list = Vertex.Map.to_alist weights in
  let res_opt = List.max_elt w_list ~compare:(fun (_, w1) (_, w2) -> Int.compare w1 w2) in
  match res_opt with
  | None -> raise (Failure "max should not be called on empty")
  | Some (vmax, _) -> vmax
;;

let ordering (graph : t) : Vertex.t list =
  let n = Vertex.Map.length graph in
  let weights = initial_weight graph in
  (* let () = print_weights weights in  *)
  let wkset = Vertex.Map.key_set graph in
  let rec aux wkset weights i =
    if i = 0
    then []
    else (
      (* let (v_max, _) = Vertex.Map.max_elt_exn weights in  *)
      let v_max = weights_max_vertex weights in
      (* let () = print_string ("v_max: "^Vertex._to_string v_max ^ "\n") in *)
      let nbrs = Vertex.Map.find_exn graph v_max in
      let inter = Vertex.Set.inter nbrs wkset in
      let incr_fn ~key ~data = if Vertex.Set.mem inter key then data + 1 else data in
      let wkset' = Set.remove wkset v_max in
      let weights' = Vertex.Map.remove weights v_max in
      let weights'' = Vertex.Map.mapi weights' ~f:incr_fn in
      (*_ new changes *)
      (* let () = print_weights weights'' in *)
      v_max :: aux wkset' weights'' (i - 1))
  in
  aux wkset weights n
;;

(*_ 
    greedy coloring 
    *)

(* let print_vertex_set (vset : Vertex.Set.t) (title : string) = 
  let print_aux v = 
    let v_str = Vertex._to_string v^", "in
      print_string v_str
  in
  let () = print_string ("\n" ^ title ^ " [") in
  let () = Vertex.Set.iter vset ~f:print_aux in
    print_string ("]\n")
;; *)
let unused_color_in_nbrs (graph : t) (color_palette : color_palette_t) (v : Vertex.t) =
  let nbrs = Vertex.Map.find_exn graph v in
  (* let () = print_vertex_set nbrs ("this is nbrs of v="^Vertex._to_string v) in *)
  let fold_f (acc : int list) v =
    match Vertex.Map.find_exn color_palette v with
    | None -> acc
    | Some c -> c :: acc
  in
  let used_colors = Vertex.Set.fold nbrs ~init:[] ~f:fold_f |> Set.of_list (module Int) in
  (* let () = List.iter (Vertex.Set.fold nbrs ~init:[] ~f:fold_f) ~f:(fun c -> print_string (string_of_int c ^ ", ")) in *)
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
       (* let () =  CustomDebug.print_with_name "\n >> color palette" [sexp_of_color_palette_t color_palette] in *)
       (* let () = print_string ("\nc_new should be 1 but got: " ^ string_of_int (c_new)^"\n") in *)
       let color_palette' = Vertex.Map.update color_palette v ~f:(fun _ -> Some c_new) in
       (v, c_new) :: coloring_aux graph color_palette' vs)
;;

type debug_2 = (Vertex.t * Vertex.t) list [@@deriving sexp]
type debug_3 = Vertex.t list [@@deriving sexp]

let coloring (graph : t) : (Vertex.t * int) list =
  (* let () = print graph in *)
  let vertex_order = ordering graph in
  (* let () =
    CustomDebug.print_with_name "\n vertex order" [ sexp_of_debug_3 vertex_order ]
  in *)
  let color_palette = precolor graph in
  (* let () =
    CustomDebug.print_with_name
      "\n color palette"
      [ sexp_of_color_palette_t color_palette ]
  in *)
  coloring_aux graph color_palette vertex_order
;;

(*_ Liveness type: liveinfo_t
    *)
type liveinfo_t = Vertex.Set.t * Vertex.Set.t [@@deriving sexp]
(*_ init live in & out: [{EAX}, {}]*)

(*_ let print_liveinfo (live_res : liveinfo_t list) = 
  let print_aux (lif : liveinfo_t) = 
    let lif_str = ""
  in
  
;; *)

(* let is_use_1 (op:AS.operand) : Vertex.Set.t = 
  match op with 
    AS.Reg r -> Vertex.Set.singleton (Vertex.R r)
  | AS.Temp t -> Vertex.Set.singleton (Vertex.T t)
  | _ -> Vertex.Set.empty
;;
let is_use_2 (op1, op2) =  Vertex.Set.union (is_use_1 op1) (is_use_1 op2)
;; *)
let is_use (op_lst : AS.operand list) =
  List.filter
    ~f:(fun op ->
      match op with
      | AS.Reg _ -> true
      | AS.Temp _ -> true
      | _ -> false)
    op_lst
  |> List.filter_map ~f:Vertex.op_to_vertex_opt
  |> Vertex.Set.of_list
;;

let uses (l : AS.instr) =
  match l with
  | AS.Binop binop ->
    let op_lst =
      match binop.op with
      | AS.Div -> [ AS.Reg AS.EAX; AS.Reg AS.EDX ]
      | _ -> []
    in
    is_use (binop.lhs :: binop.rhs :: op_lst)
  | AS.Mov mv ->
    let op_lst = [ mv.src ] in
    is_use op_lst
  | _ -> Vertex.Set.empty
;;

(* let is_def (d : AS.operand) : Vertex.Set.t = (*_ result is either empty or singleton*)
  match (Vertex.op_to_vertex_opt d) with 
  None -> Vertex.Set.empty | Some v -> Vertex.Set.singleton v
;; *)
let def_opt = function
  | AS.Binop binop -> Vertex.op_to_vertex_opt binop.dest
  | AS.Mov mv -> Vertex.op_to_vertex_opt mv.dest
  | _ -> None
;;

let defs (l : AS.instr) =
  match def_opt l with
  | None -> Vertex.Set.empty
  | Some v -> Vertex.Set.singleton v
;;

let not_live = function
  | AS.Binop binop ->
    (match binop.dest with
     | AS.Reg r -> Vertex.Set.singleton (Vertex.R r)
     | AS.Temp t -> Vertex.Set.singleton (Vertex.T t)
     | _ -> Vertex.Set.empty)
  | AS.Mov mv ->
    List.filter_map [ mv.dest; mv.src ] ~f:Vertex.op_to_vertex_opt |> Vertex.Set.of_list
  | _ -> Vertex.Set.empty
;;

type liveinfo_line_t =
  { line : AS.instr option
  ; def : Vertex.t option
  ; edge_with : Vertex.Set.t
  ; livein : Vertex.Set.t
  ; liveout : Vertex.Set.t
  }

let liveinfo_line_init =
  { line = None
  ; def = None
  ; edge_with = Vertex.Set.empty
  ; livein = Vertex.Set.singleton (Vertex.R AS.EAX)
  ; liveout = Vertex.Set.empty
  }
;;

let live_analysis_aux (l : AS.instr) acc =
  match acc with
  | [] -> raise (Failure "live_analysis_aux should not take in empty initial liveinfo")
  | liveinfo_line :: _ ->
    let livein, liveout = liveinfo_line.livein, liveinfo_line.liveout in
    let livein' = Vertex.Set.diff liveout (defs l) |> Vertex.Set.union (uses l) in
    let liveout' = livein in
    let edge_with' = Vertex.Set.diff liveout' (not_live l) in
    { line = Some l
    ; def = def_opt l
    ; edge_with = edge_with'
    ; livein = livein'
    ; liveout = liveout'
    }
    :: acc
;;

(* let live_analysis_aux (l : AS.instr) in_out = 
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
;; *)
let live_analysis (prog : AS.instr list) =
  List.fold_right prog ~f:live_analysis_aux ~init:[ liveinfo_line_init ]
;;

(* type debug_1 = (Vertex.t option * (Vertex.Set.t * liveinfo_t)) list [@@deriving sexp] *)

(*_ Live ranges: an alternative way to create interference graph (banished)
    *)

(*_ mk interference graph using livenes
    *)
let all_vertex_in_line (line : AS.instr) =
  match line with
  | AS.Binop binop ->
    List.filter_map ~f:Vertex.op_to_vertex_opt [ binop.dest; binop.rhs; binop.rhs ]
    |> Vertex.Set.of_list
  | AS.Mov mv ->
    List.filter_map ~f:Vertex.op_to_vertex_opt [ mv.dest; mv.src ] |> Vertex.Set.of_list
  | _ -> Vertex.Set.empty
;;

let all_vertex_in_prog (prog : AS.instr list) =
  let fold_f acc line = Vertex.Set.union acc (all_vertex_in_line line) in
  List.fold_left prog ~init:Vertex.Set.empty ~f:fold_f
;;

let graph_empty (vertex_set : Vertex.Set.t) =
  Vertex.Map.of_key_set vertex_set ~f:(fun _ -> Vertex.Set.empty)
;;

let add_edge (graph : t) ((u, v) : Vertex.t * Vertex.t) =
  let graph =
    Vertex.Map.update graph u ~f:(fun o ->
      match o with
      | None -> Vertex.Set.singleton v
      | Some s -> Vertex.Set.add s v)
  in
  let graph =
    Vertex.Map.update graph v ~f:(fun o ->
      match o with
      | None -> Vertex.Set.singleton u
      | Some s -> Vertex.Set.add s u)
  in
  graph
;;

let from_list graph_init (edge_list : (Vertex.t * Vertex.t) list) =
  List.fold edge_list ~init:graph_init ~f:add_edge
;;

let mk_interfere_graph (prog : AS.instr list) =
  let live_info = live_analysis prog in
  let map_f liveinfo_line =
    match liveinfo_line.def with
    | None -> None
    | Some v -> Some (v, liveinfo_line.edge_with)
  in
  let live_edge = List.map live_info ~f:map_f in
  let fold_f acc inter_info =
    match inter_info with
    | None -> acc
    | Some (v, inter_set) ->
      let new_edges = Vertex.Set.to_list inter_set |> List.map ~f:(fun u -> v, u) in
      new_edges :: acc
  in
  let edge_list_list = List.fold live_edge ~init:[] ~f:fold_f in
  let edge_list = List.concat edge_list_list in
  let all_vertex = all_vertex_in_prog prog in
  let graph_init = graph_empty all_vertex in
  (* let () = CustomDebug.print_with_name "\nedge_list" [sexp_of_debug_2 edge_list] in *)
  from_list graph_init edge_list
;;
