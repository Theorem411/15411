open Core
module AS = Assem_l4
module V = Graph.Vertex
module VM = Graph.Vertex.Map
module TM = Temp.Map
module UF = Unionfind

type color = (int[@deriving compare, equal, hash])

let coalesce_off = false

type t =
  { graph : Graph.new_graph
  ; v2c : color VM.t
  ; updated : V.t -> V.t
  }

(* let print_info s = prerr_endline s; *)

module VT = Hashtbl.Make (V)
module TT = Hashtbl.Make (Temp)
module IntSet = Set.Make (Int)

let rec find_mex (s : IntSet.t) (n : int) =
  if not (IntSet.mem s n) then n else find_mex s (n + 1)
;;

let mex_color forest (v2c : color VT.t) (new_neigh : V.Set.t) : color =
  let key_list = V.Set.to_list new_neigh in
  let updated_key_list = List.map key_list ~f:(UF.find forest) in
  let cols = List.map ~f:(VT.find_exn v2c) updated_key_list in
  find_mex (IntSet.of_list cols) 0
;;

let coalesce (g : Graph.new_graph) (__v2c : color VM.t) (f : AS.fspace) : t =
  if coalesce_off
  then { graph = g; v2c = __v2c; updated = (fun (x : V.t) : V.t -> x) }
  else (
    let forest = UF.create_forest () in
    let v2c = VT.of_alist_exn (VM.to_alist __v2c) in
    List.iter f.fdef_blocks ~f:(fun b ->
        List.iter b.block ~f:(fun l ->
            let process a b =
              if not (Graph.can_coalesce g a b)
              then ()
              else (
                match a, b with
                | V.T _, V.T _ ->
                  let t3 = Temp.create () in
                  prerr_endline
                    (sprintf
                       "Coalesing %s and %s -> %s"
                       (V._to_string a)
                       (V._to_string b)
                       (Temp.name t3));
                  (* if not same color then do:  *)
                  let col_a, col_b = VT.find_exn v2c a, VT.find_exn v2c b in
                  (* - Colesce two vertices into t3 in graph *)
                  let new_n = Graph.coalesce g (a, b) (V.T t3) in
                  (* - connect the temps in the forest *)
                  let c =
                    if equal col_b col_a then col_a else mex_color forest v2c new_n
                  in
                  UF.union_to forest (a, b) (V.T t3);
                  (* recolor the vertex *)
                  VT.remove v2c a;
                  VT.remove v2c b;
                  VT.add_exn v2c ~key:(V.T t3) ~data:c;
                  ()
                | V.R _, V.T _ ->
                  (*r <- t*)
                  prerr_endline
                    (sprintf
                       "Coalesing %s and %s -> %s"
                       (V._to_string a)
                       (V._to_string b)
                       (V._to_string a));
                  (* if not same color then do:  *)
                  let col_a, col_b = VT.find_exn v2c a, VT.find_exn v2c b in
                  (* - Colesce two vertices into t3 in graph *)
                  let new_n = Graph.coalesce g (a, b) a in
                  (* - connect the temps in the forest *)
                  let c =
                    if equal col_b col_a then col_a else mex_color forest v2c new_n
                  in
                  prerr_endline
                  (sprintf
                     "Coalesing %s[%d] and %s[%d] -> %s[%d]"
                     (V._to_string a) col_a
                     (V._to_string b) col_b
                     (V._to_string a) c);
                  UF.union_to_x forest ~x:a ~y:b;
                  (* recolor the vertex *)
                  VT.remove v2c a;
                  VT.remove v2c b;
                  VT.add_exn v2c ~key:a ~data:c;
                  ()
                | V.T _, V.R _ -> ()
                | _ -> failwith "coalseing two regs???")
            in
            match l with
            | AS.Mov { dest = AS.Temp t1; src = AS.Temp t2; _ } ->
              (* - skip this iteration if a = b (or change coalesce) *)
              process (UF.find forest (V.T t1)) (UF.find forest (V.T t2))
            (* | AS.Mov { dest = AS.Reg r1; src = AS.Temp t2; _ }
            | AS.Mov { dest = AS.Temp t2; src = AS.Reg r1; _ } ->
              (* get final vertices a, b  *)
              let a, b = UF.find forest (V.R r1), UF.find forest (V.T t2) in
              (* - skip this iteration if a = b (or change coalesce) *)
              process a b *)
            | _ -> ()));
    let old_new_names = UF.get_final_parents forest in
    (* print_info
    (sprintf
       "old_new_names [%s]"
       (String.concat
          ~sep:","
          (List.map old_new_names ~f:(fun (t1, t2) ->
               sprintf "(%s -> %s)" (Temp.name t1) (Temp.name t2))))); *)
    let update_map = VM.of_alist_exn old_new_names in
    let up t =
      match VM.find update_map t with
      | None -> t
      | Some tnew -> tnew
    in
    (* print_info (sprintf "new_fpace = %s" (AS.format_program [ new_space ])); *)
    { graph = g; v2c = VM.of_hashtbl_exn v2c; updated = up })
;;
