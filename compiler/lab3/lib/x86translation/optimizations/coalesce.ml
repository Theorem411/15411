open Core
module AS = Assem_l4
module V = Graph.Vertex
module VM = Graph.Vertex.Map
module TM = Temp.Map
module VT = Graph.VertexTable
module UF = Unionfind

type color = (int[@deriving compare, equal, hash])

let coalesce_off = false
let coalesce_regs = true

type t =
  { graph : Graph.new_graph
  ; v2c : color VM.t
  ; fspace : AS.fspace
  }

(* let print_info s = prerr_endline s; *)

module TT = Hashtbl.Make (Temp)
module IntSet = Set.Make (Int)

let rec find_mex (s : IntSet.t) (n : int) =
  if not (IntSet.mem s n) then n else find_mex s (n + 1)
;;

let mex_color forest (v2c : color VT.t) (new_neigh : V.Set.t) : color =
  let key_list = V.Set.to_list new_neigh in
  let updated_key_list =
    List.map key_list ~f:(fun v ->
        match v with
        | V.T t -> V.T (UF.find forest t)
        | v -> v)
  in
  let cols = List.map ~f:(VT.find_exn v2c) updated_key_list in
  find_mex (IntSet.of_list cols) 0
;;

let new_lbl up (lbl : Label.bt) =
  match lbl with
  | Label.BlockLbl _ -> lbl
  | Label.FunName f -> Label.FunName { f with args = List.map f.args ~f:up }
;;

let coalesce (g : Graph.new_graph) (v2c : color VT.t) (moves : (V.t * V.t) list)
    : V.t TM.t
  =
  if coalesce_off
  then TM.of_alist_exn []
  else (
    let forest = UF.create_forest () in
    let process_t_t a b =
      let t3 = Temp.create () in
      prerr_endline
        (sprintf
           "Coalesing temps: %s and %s -> %s"
           (V._to_string a)
           (V._to_string b)
           (Temp.name t3));
      (* if not same color then do:  *)
      let col_a, col_b = VT.find_exn v2c a, VT.find_exn v2c b in
      (* - Colesce two vertices into t3 in graph *)
      let new_n = Graph.coalesce g (a, b) (V.T t3) in
      (* - connect the temps in the forest *)
      let c = if equal col_b col_a then col_a else mex_color forest v2c new_n in
      UF.union_to forest (a, b) (V.T t3);
      (* recolor the vertex *)
      VT.remove v2c a;
      VT.remove v2c b;
      VT.add_exn v2c ~key:(V.T t3) ~data:c;
      ()
    in
    let process_r_t r t =
      (* BS, TODO, rewrite this function *)
      let t3 = Temp.create () in
      prerr_endline
        (sprintf "Coalesing reg: %s and temp: %s." (V._to_string r) (V._to_string t));
      (* if not same color then do:  *)
      let col_a, col_b = VT.find_exn v2c r, VT.find_exn v2c t in
      (* - Colesce two vertices into t3 in graph *)
      let new_n = Graph.coalesce g (r, t) (V.T t3) in
      (* - connect the temps in the forest *)
      let c = if equal col_b col_a then col_a else mex_color forest v2c new_n in
      UF.union_to forest (r, t) (V.T t3);
      (* recolor the vertex *)
      VT.remove v2c r;
      VT.remove v2c t;
      VT.add_exn v2c ~key:(V.T t3) ~data:c;
      ()
    in
    let process a b =
      if not (Graph.can_coalesce g a b)
      then ()
      else (
        match a, b with
        | V.T _, V.T _ -> process_t_t a b
        | V.R _, V.T _ -> if coalesce_regs then process_r_t a b else ()
        | V.T _, V.R _ -> if coalesce_regs then process_r_t b a else ()
        | _ -> failwith "coalseing two regs???")
    in
    let old_new_names = UF.get_final_parents forest in
    TM.of_alist_exn [])
;;

(* print_info
      (sprintf
         "old_new_names [%s]"
         (String.concat
            ~sep:","
            (List.map old_new_names ~f:(fun (t1, t2) ->
                 sprintf "(%s -> %s)" (Temp.name t1) (Temp.name t2))))); *)