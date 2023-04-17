open Core
module AS = Assem_l4
module G = Graph
module V = G.Vertex
module VM = V.Map
module TM = Temp.Map
module VT = G.VertexTable
module UF = Unionfind

type color = (int[@deriving compare, equal, hash])

let coalesce_on = false
let coalesce_regs = true

(* let print_info s = prerr_endline s; *)

module TT = Hashtbl.Make (Temp)
module IntSet = Set.Make (Int)

let rec find_mex ?(ban = -1) (s : IntSet.t) (n : int) =
  if not (ban = n || IntSet.mem s n) then n else find_mex ~ban s (n + 1)
;;

let mex_color ?(ban = -1) forest (v2c : color VT.t) (neigh : V.Set.t) : color =
  let key_list = V.Set.to_list neigh in
  let updated_key_list = List.map key_list ~f:(UF.find forest) in
  let cols = List.map ~f:(VT.find_exn v2c) updated_key_list in
  find_mex ~ban (IntSet.of_list cols) 0
;;

let can_coalesce ~(g : G.new_graph) (a : V.t) (b : V.t) =
  if V.equal a b
  then false
  else (
    let a_set = VT.find_exn g a in
    let b_set = VT.find_exn g b in
    (* if interfere then can not coalesce *)
    if V.Set.exists a_set ~f:(fun x -> V.equal x b)
    then false
    else (
      let hard_reg_aux (temp_set : V.Set.t) (reg_set : V.Set.t) =
        let work_set = Set.diff temp_set reg_set in
        Set.for_all work_set ~f:(fun v ->
            V.Set.length (VT.find_exn g v) < AS.num_regs - 1)
      in
      match a, b with
      | T _, T _ -> V.Set.length (V.Set.union a_set b_set) < AS.num_regs
      | R _, T _ -> hard_reg_aux b_set a_set
      | T _, R _ -> hard_reg_aux a_set b_set
      | R _, R _ -> false))
;;

let recolor_from
    ~forest
    ~(g : G.new_graph)
    ~(v2c : color VT.t)
    (reg_col : color)
    (v : V.t)
  =
  let neigh_set = VT.find_exn g v in
  let old_col = VT.find_exn v2c v in
  if not (old_col = reg_col)
  then ()
  else (
    let new_col = mex_color forest v2c neigh_set ~ban:reg_col in
    VT.change v2c v ~f:(fun _ -> Some new_col))
;;

let coalesce (g : Graph.new_graph) (v2c : color VT.t) (moves : (V.t * V.t) list)
    : V.t TM.t
  =
  if not coalesce_on
  then TM.of_alist_exn []
  else (
    let forest = UF.create_forest () in
    let process_t_t a b =
      let t3 = Temp.create () in
      (* prerr_endline
        (sprintf
           "Coalesing temps: %s and %s -> %s"
           (V._to_string a)
           (V._to_string b)
           (Temp.name t3)); *)
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
      (* prerr_endline "coloring table:"; *)
      (* prerr_endline
        (sprintf
           "[%s]"
           (String.concat
              ~sep:","
              (List.map (VT.to_alist v2c) ~f:(fun (v, c) ->
                   sprintf "%s -> %d" (V._to_string v) c)))); *)
      ()
    in
    let process_r_t r t =
      (* prerr_endline
        (sprintf "Coalesing reg: %s and temp: %s." (V._to_string r) (V._to_string t)); *)
      (* if not same color then do:  *)
      (* - Colesce two vertices into t3 in graph *)
      let temp_set, reg_set = VT.find_exn g t, VT.find_exn g r in
      let recolor_set = Set.diff temp_set reg_set in
      let reg_col = VT.find_exn v2c r in
      let (_ : V.Set.t) = Graph.coalesce g (r, t) r in
      (* - connect the temps in the forest *)
      UF.union_to_x forest ~x:r ~y:t;
      (* just remove the coloring of temp *)
      VT.remove v2c t;
      V.Set.iter recolor_set ~f:(recolor_from ~forest ~g ~v2c reg_col);
      (* prerr_endline "coloring table:";
      prerr_endline
        (sprintf
           "[%s]"
           (String.concat
              ~sep:","
              (List.map (VT.to_alist v2c) ~f:(fun (v, c) ->
                   sprintf "%s -> %d" (V._to_string v) c)))); *)
      ()
    in
    let process a b =
      let a, b = UF.find forest a, UF.find forest b in
      if can_coalesce ~g a b
      then (
        match a, b with
        | V.T _, V.T _ -> process_t_t a b
        | V.R _, V.T _ -> if coalesce_regs then process_r_t a b else ()
        | V.T _, V.R _ -> if coalesce_regs then process_r_t b a else ()
        | _ -> failwith "coalseing two regs???")
      else ()
      (* prerr_endline
          (sprintf "Can not coalesce %s and %s" (V._to_string a) (V._to_string b)) *)
    in
    List.iter moves ~f:(fun (a, b) -> process a b);
    let roots = UF.get_final_parents forest in
    let temp_final_versions =
      List.filter_map roots ~f:(fun (v, p) ->
          match v, p with
          | V.T t, _ -> Some (t, p)
          | V.R r1, V.R r2 ->
            if AS.equal_reg r1 r2 then None else failwith "merged two regs. "
          | V.R _, V.T _ -> failwith "Register's parent can not be a temp")
    in
    (* prerr_endline
      (sprintf
         "old_new_names [%s]"
         (String.concat
            ~sep:","
            (List.map temp_final_versions ~f:(fun (t1, t2) ->
                 sprintf "(%s -> %s)" (Temp.name t1) (V._to_string t2))))); *)
    TM.of_alist_exn temp_final_versions)
;;

(* print_info
      (sprintf
         "old_new_names [%s]"
         (String.concat
            ~sep:","
            (List.map old_new_names ~f:(fun (t1, t2) ->
                 sprintf "(%s -> %s)" (Temp.name t1) (Temp.name t2))))); *)