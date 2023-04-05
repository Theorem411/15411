open Core
module AS = Assem_l4
module V = Graph.Vertex
module VM = Graph.Vertex.Map
module TM = Temp.Map
module R = Register
module Regalloc = Regalloc
module UF = Unionfind

type color = (int[@deriving compare, equal, hash])

type t =
  { graph : Graph.new_graph
  ; v2c : color VM.t
  ; fspace : AS.fspace
  }

module VT = Hashtbl.Make (V)
module TT = Hashtbl.Make (Temp)
module IntSet = Set.Make (Int)

let rec find_mex (s : IntSet.t) (n : int) =
  if not (IntSet.mem s n) then n else find_mex s (n + 1)
;;

let mex_color (v2c : color VT.t) (new_neigh : V.Set.t) : color =
  let key_list = V.Set.to_list new_neigh in
  let cols = List.map ~f:(VT.find_exn v2c) key_list in
  find_mex (IntSet.of_list cols) 0
;;

let update_instr (f : Temp.t -> Temp.t) (instr : AS.instr) =
  let up op : AS.operand =
    match op with
    | AS.Temp t -> AS.Temp (f t)
    | _ -> op
  in
  let up_list tmps = List.map tmps ~f:(fun (t, sz) -> f t, sz) in
  match instr with
  | AS.PureBinop { op; size; dest; lhs; rhs } ->
    AS.PureBinop { op; size; dest = up dest; lhs = up lhs; rhs = up rhs }
  | AS.EfktBinop { op; dest; lhs; rhs } ->
    AS.EfktBinop { op; dest = up dest; lhs = up lhs; rhs = up rhs }
  | AS.Unop { op; dest } -> AS.Unop { op; dest = up dest }
  | AS.Mov { dest; size; src } -> AS.Mov { dest = up dest; size; src = up src }
  | AS.MovSxd { dest; src } -> AS.MovSxd { dest = up dest; src = up src }
  | AS.MovFrom { dest; size; src } -> AS.MovFrom { dest = up dest; size; src = up src }
  | AS.MovTo { dest; size; src } -> AS.MovTo { dest = up dest; size; src = up src }
  | AS.Set { typ; src } -> AS.Set { typ; src = up src }
  | AS.Cmp { size; lhs; rhs } -> AS.Cmp { size; lhs = up lhs; rhs = up rhs }
  | AS.LoadFromStack temps -> AS.LoadFromStack (up_list temps)
  | AS.Call { fname; args_in_regs; args_overflow } ->
    AS.Call { fname; args_in_regs; args_overflow = up_list args_overflow }
  | AS.Jmp _
  | AS.Cjmp _
  | AS.Ret
  | AS.Lab _
  | AS.AssertFail
  | AS.Directive _
  | AS.Comment _ -> instr
;;

let coalesce (g : Graph.new_graph) (__v2c : color VM.t) (f : AS.fspace) : t =
  let forest = UF.create_forest () in
  let v2c = VT.of_alist_exn (VM.to_alist __v2c) in
  List.iter f.fdef_blocks ~f:(fun b ->
      List.iter b.block ~f:(fun l ->
          match l with
          | AS.Mov { dest = AS.Temp t1; src = AS.Temp t2; _ } ->
            (* get final vertices a, b  *)
            let a, b = UF.find forest t1, UF.find forest t2 in
            (* - skip this iteration if a = b (or change coalesce) *)
            if not (Graph.can_coalesce g (V.T a) (V.T b))
            then ()
            else (
              (* if not same color then do:  *)
              let col_a, col_b = VT.find_exn v2c (V.T a), VT.find_exn v2c (V.T b) in
              (* - Colesce two vertices into t3 in graph *)
              let t3 = Temp.create () in
              let new_n = Graph.coalesce g (V.T a, V.T b) (V.T t3) in
              (* - connect the temps in the forest *)
              UF.union forest a b;
              let c = if equal col_b col_a then col_a else mex_color v2c new_n in
              (* recolor the vertex *)
              VT.remove v2c (V.T a);
              VT.remove v2c (V.T b);
              VT.add_exn v2c ~key:(V.T t3) ~data:c;
              ())
          | _ -> ()));
  let old_new_names = UF.get_final_parents forest in
  let update_map = TM.of_alist_exn old_new_names in
  let up t =
    match TM.find update_map t with
    | None -> t
    | Some tnew -> tnew
  in
  let new_fdef =
    List.map f.fdef_blocks ~f:(fun b ->
        let new_instrs : AS.instr list = List.map b.block ~f:(update_instr up) in
        { b with block = new_instrs })
  in
  let new_space : AS.fspace =
    { fname = f.fname
    ; fdef_blocks = new_fdef
    ; args = List.map f.args ~f:(fun (t, sz) -> up t, sz)
    }
  in
  { graph = g; v2c = VM.of_hashtbl_exn v2c; fspace = new_space }
;;
