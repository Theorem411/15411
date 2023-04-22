open Core
module AS = Assem_l4

let strength_instr (instr : AS.instr) : AS.instr =
  match instr with
  | AS.PureBinop { op = AS.Mul; size = AS.S; dest; lhs = AS.Imm c1; rhs = AS.Imm c2 } ->
    let c = Int64.( * ) c1 c2 in
    (match Int32.of_int64 c with
    | None -> instr
    | Some _ -> AS.Mov { dest; src = AS.Imm c; size = AS.S })
  | AS.PureBinop { op = AS.Mul; size = AS.S; dest; lhs = AS.Imm c; rhs } ->
    if Int64.( = ) c Int64.zero
    then AS.Mov { dest; size = AS.S; src = AS.Imm Int64.zero }
    else if Int64.( > ) c Int64.zero
    then (
      let k_up, k_down = Int64.ceil_log2 c, Int64.floor_log2 c in
      if k_up <> k_down
      then instr
      else (
        let k = AS.Imm (Int64.of_int_exn k_up) in
        AS.EfktBinop { op = AS.ShiftL; dest; lhs = rhs; rhs = k }))
    else instr
  | AS.PureBinop { op = AS.Mul; size = AS.S; dest; lhs; rhs = AS.Imm c } ->
    if Int64.( = ) c Int64.zero
    then AS.Mov { dest; size = AS.S; src = AS.Imm Int64.zero }
    else if Int64.( > ) c Int64.zero
    then (
      let k_up, k_down = Int64.ceil_log2 c, Int64.floor_log2 c in
      if k_up <> k_down
      then instr
      else (
        let k = AS.Imm (Int64.of_int_exn k_up) in
        AS.EfktBinop { op = AS.ShiftL; dest; lhs; rhs = k }))
    else instr
  | AS.PureBinop { op = AS.Mul; size = AS.L; dest; lhs = AS.Imm c; _ } ->
    if Int64.( = ) c Int64.zero
    then AS.Mov { dest; size = AS.L; src = AS.Imm Int64.zero }
    else instr
  | AS.PureBinop { op = AS.Mul; size = AS.L; dest; rhs = AS.Imm c; _ } ->
    if Int64.( = ) c Int64.zero
    then AS.Mov { dest; size = AS.L; src = AS.Imm Int64.zero }
    else instr
  (* | LLVM_Jmp _ | LLVM_Call _ | LLVM_Cmp _ | LLVM_IF _ | LLVM_Ret _ -> failwith "not implemented yet" *)
  | _ -> instr
;;

let strength_block (block : AS.block) : AS.block =
  { block with block = List.map block.block ~f:strength_instr }
;;

let strength_fspace (fspace : AS.fspace) : AS.fspace =
  { fspace with fdef_blocks = List.map fspace.fdef_blocks ~f:strength_block }
;;

let strength (prog : AS.program) : AS.program = List.map prog ~f:strength_fspace