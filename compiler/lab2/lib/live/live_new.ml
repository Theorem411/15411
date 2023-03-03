(* open Core
module AS = Assem_new

module V = Graph.Vertex
let def (instr : AS.instr) : V.Set.t = 
  match instr with 
  | AS.Mov { dest; src; } -> failwith "not implemented"
  | AS.Unop { op; dest; } -> failwith "not implemented"
  | AS.PureBinop { op; dest; lhs; rhs; } -> failwith "not implemented"
  | AS.EfktBinop { op; dest; lhs; rhs; } -> failwith "not implemented"
  | AS.Jmp _ | AS.Cjmp _ -> V.Set.empty
  | AS.Ret -> failwith "not implemented"
  | AS.Lab _ -> failwith "not implemented"
  | AS.Cmp (o1, o2) -> failwith "not implemented"
  | AS.AssertFail -> V.Set.empty
  | AS.Set { typ; src; } -> failwith "not implemented"
  | AS.Call {fname; args_overflow; } -> failwith "not implemented"
  | AS.Directive _ | Comment _ -> V.Set.empty

let use (instr : AS.instr) : V.Set.t = 
  match instr with 
  | AS.Mov { dest; src; } -> failwith "not implemented"
  | AS.Unop { op; dest; } -> failwith "not implemented"
  | AS.PureBinop { op; dest; lhs; rhs; } -> failwith "not implemented"
  | AS.EfktBinop { op; dest; lhs; rhs; } -> failwith "not implemented"
  | AS.Jmp _ | AS.Cjmp _ -> V.Set.empty
  | AS.Ret -> failwith "not implemented"
  | AS.Lab _ -> failwith "not implemented"
  | AS.Cmp (o1, o2) -> failwith "not implemented"
  | AS.AssertFail -> V.Set.empty
  | AS.Set { typ; src; } -> failwith "not implemented"
  | AS.Call {fname; args_overflow; } -> failwith "not implemented"
  | AS.Directive _ | Comment _ -> V.Set.empty

type qcell = 
  { block : AS.block
  ; preds : Label.t list
  }

type mcell = 
  { block : AS.block
  ; defs : V.Set.t
  ; uses : V.Set.t
  } *)