open Core
module AS = Assem

(* type line = int * AS.instr *)

module V = Graph.Vertex
module IntSet = Set.Make (Int)
module IntTable = Hashtbl.Make (Int)
module VertexTable = Hashtbl.Make (V)

type __graph_t = (V.t, V.Set.t) Hashtbl.t
type live_t = (int, V.Set.t) Hashtbl.t

let v_of_op op =
  match Graph.Vertex.op_to_vertex_opt op with
  | None -> raise (Failure "op is not supported")
  | Some r -> r
;;

(** Define set *)
let def (_ : int) instr : V.Set.t =
  match instr with
  | AS.Mov { dest = d; _ } -> V.Set.of_list [ v_of_op d ]
  | Binop { op = Div | Mod; dest = d; _ } ->
    V.Set.of_list [ V.R AS.EAX; V.R AS.EDX; v_of_op d ]
  | Binop { dest = d; _ } -> V.Set.of_list [ v_of_op d ]
  | Directive _ -> V.Set.empty
  | Comment _ -> V.Set.empty
;;

(** Use set *)
let use (_ : int) instr : V.Set.t =
  V.Set.of_list
    (List.filter_opt
       (match instr with
       | AS.Mov { src = s; _ } -> [ Graph.Vertex.op_to_vertex_opt s ]
       | Binop { lhs = l; rhs = r; _ } ->
         [ Graph.Vertex.op_to_vertex_opt l; Graph.Vertex.op_to_vertex_opt r ]
       | Directive _ -> []
       | Comment _ -> []))
;;

let get_init (_ : int) : live_t * live_t =
  let empty_1 : live_t = IntTable.create () in
  let empty_2 : live_t = IntTable.create () in
  empty_1, empty_2
;;

(* let print_vset (es : V.Set.t) =
  print_string "[";
  V.Set.iter
    ~f:(fun v ->
      V.print v;
      print_string ", ")
    es;
  print_string "]"
;; *)

let main (instrs : AS.instr list) _ =
  let n = List.length instrs in
  let enum = List.mapi ~f:(fun i instr -> i, instr) instrs in
  let defs = List.mapi ~f:def instrs in
  let uses = List.mapi ~f:use instrs in
  (* gets current live_in and live_out, updates them accordingly *)
  let (live_in, live_out) : live_t * live_t = get_init n in
  let liveness_aux (i, _) () =
    let d = List.nth_exn defs i in
    let u = List.nth_exn uses i in
    (* let all_succs = succ i in *)
    let live_out_i =
      match i = n - 1 with
      | true -> V.Set.empty
      | false -> IntTable.find_exn live_in (i + 1)
    in
    let live_in_i = V.Set.union u (V.Set.diff live_out_i d) in
    let () =
      match IntTable.add live_in ~key:i ~data:live_in_i with
      | _ -> ()
    in
    let () =
      match IntTable.add live_out ~key:i ~data:live_out_i with
      | _ -> ()
    in
    ()
  in
  let () = List.fold_right enum ~f:liveness_aux ~init:() in
  let indicies = List.range ~start:`inclusive ~stop:`exclusive 0 n in
  let lin_list = List.map ~f:(fun i -> IntTable.find_exn live_in i) indicies in
  let lout_list = List.map ~f:(fun i -> IntTable.find_exn live_out i) indicies in
  defs, uses, lin_list, lout_list
;;

let get_vertices (instrs : AS.instr list) : V.Set.t =
  let to_v_aux instr : V.t list =
    List.filter_opt
      (match instr with
      | AS.Mov { dest = d; src = s } -> List.map ~f:V.op_to_vertex_opt [ d; s ]
      | Binop { op = Div | Mod; dest = d; lhs = l; rhs = r } ->
        List.map ~f:V.op_to_vertex_opt ([ d; l; r ] @ [ AS.Reg AS.EAX; AS.Reg AS.EDX ])
      | Binop { dest = d; lhs = l; rhs = r; _ } ->
        List.map ~f:V.op_to_vertex_opt [ d; l; r ]
      | Directive _ -> []
      | Comment _ -> [])
  in
  V.Set.of_list (List.concat_map ~f:to_v_aux instrs)
;;

(*_ https://stackoverflow.com/questions/10893521/how-to-take-product-of-two-list-in-ocaml *)
let cartesian l l' =
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> e, e') l') l)
;;

let get_edges (d : V.Set.t list) (live_out : V.Set.t list) (live_in : V.Set.t list)
    : (V.t * V.t) list
  =
  (* get edges from the single line *)
  let get_edges_aux (d' : V.Set.t) (live_out' : V.Set.t) =
    cartesian (V.Set.to_list d') (V.Set.to_list live_out')
  in
  (* combine all *)
  List.concat (List.map2_exn ~f:get_edges_aux d live_out)
  @ List.concat (List.map2_exn ~f:get_edges_aux d live_in)
;;

(* TODO: add dedup *)

let create_graph (vertices : V.Set.t) (edges : (V.t * V.t) list) =
  (* hash table graph *)
  let graph' : __graph_t = VertexTable.create () in
  let () =
    V.Set.iter vertices ~f:(fun v -> VertexTable.set graph' ~key:v ~data:V.Set.empty)
  in
  let () =
    List.iter edges ~f:(fun (a, b) ->
        let a_set = VertexTable.find_exn graph' a in
        let b_set = VertexTable.find_exn graph' b in
        if not (V.equal a b)
        then (
          let () = VertexTable.set graph' ~key:b ~data:(V.Set.add b_set a) in
          let () = VertexTable.set graph' ~key:a ~data:(V.Set.add a_set b) in
          ())
        else ())
  in
  VertexTable.fold graph' ~init:V.Map.empty ~f:(fun ~key ~data m ->
      V.Map.add_exn m ~key ~data)
;;

(* let print_set_list (es : V.Set.t list) =
  let () =
    List.iteri es ~f:(fun i a ->
        print_string (string_of_int (i + 3) ^ ":");
        print_vset a;
        print_string "\n")
  in
  print_endline ""
;; *)

(* let print_edges (es : (V.t * V.t) list) =
  let () =
    List.iter es ~f:(fun (a, b) ->
        print_string "(";
        V.print a;
        print_string "-";
        V.print b;
        print_string ";")
  in
  print_endline ".\nDone with edges."
;; *)

let mk_graph (instrs : AS.instr list) : Graph.t =
  let n = List.length instrs in
  let succ i = if i >= n - 1 then IntSet.empty else IntSet.of_list [ i + 1 ] in
  let d, _, live_in, live_out = main instrs succ in
  (* let () = print_set_list live_out in *)
  (* let () = print_set_list d in
  let () = print_set_list live_in in *)
  let vertices = get_vertices instrs in
  let edges = get_edges d live_out live_in in
  (* let () = print_edges edges in *)
  create_graph vertices edges
;;
