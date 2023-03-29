module T = Tree_l4
module A = Assem_l4

let munch_binary_op = function
  | T.Add -> A.Add
  | T.Sub -> A.Sub
  | T.Mul -> A.Mul
  | T.BitAnd -> A.BitAnd
  | T.BitOr -> A.BitOr
  | T.BitXor -> A.BitXor
;;

let munch_unary_op = function
  | T.BitNot -> A.BitNot
;;

let cmp_to_set_t = function
  | T.Eq -> A.Sete
  | T.Neq -> A.Setne
  | T.Less -> A.Setl
  | T.Leq -> A.Setle
  | T.Greater -> A.Setg
  | T.Geq -> A.Setge
;;

let munch_efkt_op = function
  | T.Div -> A.Div
  | T.Mod -> A.Mod
  | T.ShftL -> A.ShiftL
  | T.ShftR -> A.ShiftR
;;

let munch_size = function
  | 4 -> A.S
  | 8 -> A.L
  | _ -> failwith "codegen: size is not 4 or 8"
;;

let munch_exp : A.operand -> T.mpexp -> A.instr list =
  let rec munch_exp_rev (dest : A.operand) (exp : T.mpexp) : A.instr list =
    let e, esize = exp in
    match e with
    | T.Const n -> [ A.Mov { dest; size = A.S; src = A.Imm n } ]
    | T.Temp t -> [ A.Mov { dest; size = munch_size esize; src = A.Temp t } ]
    | T.Binop { op; lhs; rhs } ->
      let op = munch_binary_op op in
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      A.PureBinop { op; dest; size = A.S; lhs = t1; rhs = t2 }
      :: (munch_exp_rev t2 lhs @ munch_exp_rev t1 rhs)
    | T.Cmpop { op; size; lhs = e1; rhs = e2 } ->
      let typ = cmp_to_set_t op in
      let lhs = A.Temp (Temp.create ()) in
      let rhs = A.Temp (Temp.create ()) in
      (*_ view in reverse order *)
      A.Set { typ; src = dest }
      :: A.Cmp { lhs; rhs; size = munch_size size }
      :: (munch_exp_rev rhs e2 @ munch_exp_rev lhs e1)
    | T.Unop { op; p } ->
      let op = munch_unary_op op in
      A.Unop { op; dest } :: munch_exp_rev dest p
    | T.Mem T.Null -> [ A.Jmp A.mem_fail_lab ]
    | T.Mem (T.Ptr { start; off }) ->
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let zero8 = A.Imm (Int32.of_int 0) in
      let off8 = A.Imm (Int32.of_int off) in
      let size = munch_size off in
      [ A.MovFrom { dest; src = t2; size }
      ; A.Cjmp { typ = A.Je; l = A.mem_fail_lab }
      ; A.Cmp { lhs = t2; rhs = zero8; size = A.L }
      ; A.PureBinop { op = A.Add; size = A.L; dest = t2; lhs = t1; rhs = off8 }
      ]
      @ munch_exp_rev t1 start
    | T.Mem (T.Arr { head; idx; typ_size; extra }) ->
      let a = A.Temp (Temp.create ()) in
      let b = A.Temp (Temp.create ()) in
      let zero8 = A.Imm (Int32.of_int 0) in
      let chk_head_null_rev =
        [ A.Cmp { size = A.L; lhs = a; rhs = zero8 }
        ; A.Cjmp { typ = A.Je; l = A.mem_fail_lab }
        ]
        |> List.rev
      in
      let chk_idx_pos_rev =
        [ A.Cmp { size = A.S; lhs = b; rhs = zero8 }
        ; A.Cjmp { typ = A.Jl; l = A.mem_fail_lab }
        ]
        |> List.rev
      in
      let t3 = A.Temp (Temp.create ()) in
      let t4 = A.Temp (Temp.create ()) in
      let eight8 = A.Imm (Int32.of_int 8) in
      let chk_idx_len_rev =
        [ A.PureBinop { op = A.Sub; size = A.L; dest = t3; lhs = a; rhs = eight8 }
        ; A.MovFrom { dest = t4; size = A.S; src = t3 }
        ; A.Cmp { size = A.S; lhs = b; rhs = t4 }
        ; A.Cjmp { typ = A.Jge; l = A.mem_fail_lab }
        ]
        |> List.rev
      in
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let res_rev =
        [ A.Mov { dest = t1; size = A.L; src = a }
        ; A.MovSxd { dest = b; src = b }
        ; A.PureBinop
            { op = A.Mul
            ; size = A.L
            ; dest = t2
            ; lhs = b
            ; rhs = A.Imm (Int32.of_int typ_size)
            }
        ; A.PureBinop { op = A.Add; size = A.L; dest = t1; lhs = t1; rhs = t1 }
        ; A.PureBinop
            { op = A.Add
            ; size = A.L
            ; dest = t1
            ; lhs = t1
            ; rhs = A.Imm (Int32.of_int extra)
            }
        ; A.MovFrom { dest; size = munch_size esize; src = t1 }
        ]
        |> List.rev
      in
      (*_ view in reverse order *)
      [ res_rev
      ; chk_idx_len_rev
      ; chk_idx_pos_rev
      ; chk_head_null_rev
      ; munch_exp_rev b idx
      ; munch_exp_rev a head
      ]
      |> List.concat
    | T.Addr T.Null -> [ A.Mov { dest; size = A.L; src = A.Imm (Int32.of_int 0) } ]
    | T.Addr (T.Ptr { start; off }) ->
      let t = A.Temp (Temp.create ()) in
      A.PureBinop
        { op = A.Add; size = A.L; dest; lhs = t; rhs = A.Imm (Int32.of_int off) }
      :: munch_exp_rev t start
    | T.Addr (T.Arr { head; idx; typ_size; extra }) ->
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let a = A.Temp (Temp.create ()) in
      let b = A.Temp (Temp.create ()) in
      let res =
        [ A.MovSxd { dest = b; src = b }
        ; A.PureBinop
            { dest = t1
            ; size = A.L
            ; lhs = b
            ; op = A.Mul
            ; rhs = A.Imm (Int32.of_int typ_size)
            }
        ; A.Mov { dest = t2; size = A.L; src = a }
        ; A.PureBinop { dest = t2; size = A.L; lhs = t2; op = A.Add; rhs = t1 }
        ; A.PureBinop
            { dest = t2
            ; size = A.L
            ; lhs = t2
            ; op = A.Add
            ; rhs = A.Imm (Int32.of_int extra)
            }
        ; A.Mov { dest; size = A.L; src = t2 }
        ]
        |> List.rev
      in
      [ res; munch_exp_rev b idx; munch_exp_rev a head ] |> List.concat
    | T.Alloc size ->
      (* Implement the Alloc case *)
      failwith "Not implemented"
    | T.Calloc { len; typ } ->
      (* Implement the Calloc case *)
      failwith "Not implemented"
  in
  fun t exp -> List.rev (munch_exp_rev t exp)
;;

let munch_stm : T.stm -> A.instr list = function
  | T.If { cond; lt; lf } -> failwith "Not implemented"
  | T.Goto label -> failwith "Not implemented"
  | T.Label label -> failwith "Not implemented"
  | T.MovEfktExp { dest; ebop; lhs; rhs } -> failwith "Not implemented"
  | T.MovPureExp { dest; src } -> failwith "Not implemented"
  | T.MovFuncApp { dest; fname; args } -> failwith "Not implemented"
  | T.MovToMem { mem; src } -> failwith "Not implemented"
  | T.Return mpexp_opt -> failwith "Not implemented"
  | T.AssertFail -> failwith "Not implemented"
;;

let munch_block : T.block -> A.block = failwith "no"
let codegen : T.program -> A.program = failwith "no"
