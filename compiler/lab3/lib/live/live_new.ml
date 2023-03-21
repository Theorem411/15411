(* open Core
module AS = Assem_new
module B = Block
module V = Graph.Vertex

(*_ return def set and uses set *)
let def_n_use (instr : AS.instr) : V.Set.t * V.Set.t =
  let op_to_vset (op : AS.operand) : V.Set.t =
    match V.op_to_vertex_opt op with
    | Some v -> V.Set.singleton v
    | None -> V.Set.empty
  in
  match instr with
  | AS.Mov { dest; src } -> op_to_vset dest, op_to_vset src
  | AS.Unop { dest; _ } -> op_to_vset dest, op_to_vset dest
  | AS.PureBinop { dest; lhs; rhs; _ } ->
    op_to_vset dest, V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ]
  | AS.EfktBinop { op = AS.Div | AS.Mod; dest; lhs; rhs } ->
    ( V.Set.union_list (List.map ~f:op_to_vset [ dest; AS.Reg AS.EAX; AS.Reg AS.EDX ])
    , V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ] )
  | AS.EfktBinop { op = AS.ShiftL | AS.ShiftR; dest; lhs; rhs } ->
    ( V.Set.union_list (List.map ~f:op_to_vset [ dest; AS.Reg AS.ECX ])
    , V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ] )
  | AS.Jmp _ | AS.Cjmp _ -> V.Set.empty, V.Set.empty
  | AS.Ret -> V.Set.empty, V.Set.singleton (V.R AS.EAX)
  | AS.Lab _ -> V.Set.empty, V.Set.empty
  | AS.Cmp (o1, o2) -> V.Set.empty, V.Set.union_list [ op_to_vset o1; op_to_vset o2 ]
  | AS.AssertFail -> V.Set.empty, V.Set.empty
  | AS.Set { src; _ } -> op_to_vset src, V.Set.empty
  | AS.Call { args_in_regs; _ } ->
    ( V.Set.of_list
        (List.map
           ~f:(fun r -> V.R r)
           [ AS.EAX; AS.EDI; AS.ESI; AS.EDX; AS.ECX; AS.R8D; AS.R9D; AS.R10D; AS.R11D ])
    , V.Set.of_list (List.map ~f:(fun r -> V.R r) args_in_regs) )
  | AS.Directive _ | Comment _ -> V.Set.empty, V.Set.empty
  | AS.LoadFromStack args ->
    V.Set.of_list (List.map ~f:(fun t -> V.T t) args), V.Set.empty
;;

type qcell =
  { livein: V.Set.t
  ; liveout : V.Set.t
  }

type line_aug = {
  line: AS.instr
  ; def : V.Set.t
  ; use : V.Set.t
}

type mcell =
  { code_aug : line_aug list
  ; preds : B.block_label_t list
  }

module T = struct 
  type t = B.block_label_t
  [@@deriving compare, equal, sexp, hash]
end
module LM = Map.Make(T)
module LT = Hashtbl.Make(T)  *)