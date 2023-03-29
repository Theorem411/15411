open Core
module T = Tree_l4
module A = Assem_l4

let if_cond_to_rev_jump_t = function
  | T.Leq -> A.Jg
  | T.Less -> A.Jge
  | T.Geq -> A.Jl
  | T.Greater -> A.Jle
  | T.Eq -> A.Jne
  | T.Neq -> A.Je
;;

let arg_i_to_reg = function
  | 0 -> A.EDI
  | 1 -> A.ESI
  | 2 -> A.EDX
  | 3 -> A.ECX
  | 4 -> A.R8D
  | 5 -> A.R9D
  | _ -> failwith "args overflow 6"
;;

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

let munch_exp (dest : A.operand) (exp : T.mpexp) ~(mfl : Label.t) : A.instr list =
  let rec munch_exp_rev (dest : A.operand) (exp : T.mpexp) ~(mfl : Label.t) : A.instr list
    =
    let e, esize = exp in
    match e with
    | T.Const n -> [ A.Mov { dest; size = A.S; src = A.Imm n } ]
    | T.Temp t -> [ A.Mov { dest; size = munch_size esize; src = A.Temp t } ]
    | T.Binop { op; lhs; rhs } ->
      let op = munch_binary_op op in
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      A.PureBinop { op; dest; size = A.S; lhs = t1; rhs = t2 }
      :: (munch_exp_rev ~mfl t2 lhs @ munch_exp_rev ~mfl t1 rhs)
    | T.Cmpop { op; size; lhs = e1; rhs = e2 } ->
      let typ = cmp_to_set_t op in
      let lhs = A.Temp (Temp.create ()) in
      let rhs = A.Temp (Temp.create ()) in
      (*_ view in reverse order *)
      A.Set { typ; src = dest }
      :: A.Cmp { lhs; rhs; size = munch_size size }
      :: (munch_exp_rev ~mfl rhs e2 @ munch_exp_rev ~mfl lhs e1)
    | T.Unop { op; p } ->
      let op = munch_unary_op op in
      A.Unop { op; dest } :: munch_exp_rev ~mfl dest p
    | T.Mem T.Null -> [ A.Jmp mfl ]
    | T.Mem (T.Ptr { start; off }) ->
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let zero8 = A.Imm (Int32.of_int_exn 0) in
      let off8 = A.Imm (Int32.of_int_exn off) in
      let size = munch_size off in
      [ A.MovFrom { dest; src = t2; size }
      ; A.Cjmp { typ = A.Je; l = mfl }
      ; A.Cmp { lhs = t2; rhs = zero8; size = A.L }
      ; A.PureBinop { dest = t2; size = A.L; lhs = t1; op = A.Add; rhs = off8 }
      ]
      @ munch_exp_rev ~mfl t1 start
    | T.Mem (T.Arr { head; idx; typ_size; extra }) ->
      let a = A.Temp (Temp.create ()) in
      let b = A.Temp (Temp.create ()) in
      let zero8 = A.Imm (Int32.of_int_exn 0) in
      let chk_head_null_rev =
        [ A.Cmp { size = A.L; lhs = a; rhs = zero8 }; A.Cjmp { typ = A.Je; l = mfl } ]
        |> List.rev
      in
      let chk_idx_pos_rev =
        [ A.Cmp { size = A.S; lhs = b; rhs = zero8 }; A.Cjmp { typ = A.Jl; l = mfl } ]
        |> List.rev
      in
      let t3 = A.Temp (Temp.create ()) in
      let t4 = A.Temp (Temp.create ()) in
      let eight8 = A.Imm (Int32.of_int_exn 8) in
      let chk_idx_len_rev =
        [ A.PureBinop { op = A.Sub; size = A.L; dest = t3; lhs = a; rhs = eight8 }
        ; A.MovFrom { dest = t4; size = A.S; src = t3 }
        ; A.Cmp { size = A.S; lhs = b; rhs = t4 }
        ; A.Cjmp { typ = A.Jge; l = mfl }
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
            ; rhs = A.Imm (Int32.of_int_exn typ_size)
            }
        ; A.PureBinop { op = A.Add; size = A.L; dest = t1; lhs = t1; rhs = t1 }
        ; A.PureBinop
            { op = A.Add
            ; size = A.L
            ; dest = t1
            ; lhs = t1
            ; rhs = A.Imm (Int32.of_int_exn extra)
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
      ; munch_exp_rev ~mfl b idx
      ; munch_exp_rev ~mfl a head
      ]
      |> List.concat
    | T.Addr T.Null -> [ A.Mov { dest; size = A.L; src = A.Imm (Int32.of_int_exn 0) } ]
    | T.Addr (T.Ptr { start; off }) ->
      let t = A.Temp (Temp.create ()) in
      A.PureBinop
        { op = A.Add; size = A.L; dest; lhs = t; rhs = A.Imm (Int32.of_int_exn off) }
      :: munch_exp_rev ~mfl t start
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
            ; rhs = A.Imm (Int32.of_int_exn typ_size)
            }
        ; A.Mov { dest = t2; size = A.L; src = a }
        ; A.PureBinop { dest = t2; size = A.L; lhs = t2; op = A.Add; rhs = t1 }
        ; A.PureBinop
            { dest = t2
            ; size = A.L
            ; lhs = t2
            ; op = A.Add
            ; rhs = A.Imm (Int32.of_int_exn extra)
            }
        ; A.Mov { dest; size = A.L; src = t2 }
        ]
        |> List.rev
      in
      [ res; munch_exp_rev ~mfl b idx; munch_exp_rev ~mfl a head ] |> List.concat
  in
  List.rev (munch_exp_rev dest exp ~mfl)
;;

let munch_stm (stm : T.stm) ~(mfl : Label.t) : A.instr list =
  match stm with
  | T.MovPureExp { dest; src } -> munch_exp (A.Temp dest) src ~mfl
  | T.MovEfktExp { dest; ebop; lhs; rhs } ->
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    [ munch_exp t1 lhs ~mfl
    ; munch_exp t2 rhs ~mfl
    ; [ A.EfktBinop { dest = A.Temp dest; op = munch_efkt_op ebop; lhs = t1; rhs = t2 } ]
    ]
    |> List.concat
  | T.Goto label -> [ A.Jmp label ]
  | T.Label label -> [ A.Lab label ]
  | T.Return eopt ->
    (* return e is implemented as %eax <- e *)
    (match eopt with
     | None -> [ A.Ret ]
     | Some e -> munch_exp (A.Reg A.EAX) e ~mfl @ [ A.Ret ])
  | T.AssertFail -> [ A.AssertFail ]
  | T.Alloc { dest; size } ->
    [ (* edi <-4 size *)
      A.Mov { dest = A.Reg A.RDI; size = A.S; src = A.Imm (Int32.of_int_exn size) }
      (* call allocjavaway *)
    ; A.Call
        { fname = Symbol.symbol CustomAssembly.alloc_fname
        ; args_overflow = []
        ; args_in_regs = [ A.RDI, A.S ]
        }
      (* dest <-8 rax *)
    ; A.Mov { dest = A.Temp dest; size = A.L; src = A.Reg A.EAX }
    ]
  | T.Calloc { dest; len; typ } ->
    let t1 = A.Temp (Temp.create ()) in
    [ munch_exp t1 len ~mfl
    ; [ (* edi <-4 typ *)
        A.Mov { dest = A.Reg A.RDI; size = A.S; src = A.Imm (Int32.of_int_exn typ) }
        (* edi <-4 len *)
      ; A.Mov { dest = A.Reg A.RSI; size = A.S; src = t1 } (* call allocjavaway *)
      ; A.Call
          { fname = Symbol.symbol CustomAssembly.alloc_array_fname
          ; args_overflow = []
          ; args_in_regs = [ A.RDI, A.S; A.RSI, A.S ]
          }
        (* dest <-8 rax *)
      ; A.Mov { dest = A.Temp dest; size = A.L; src = A.Reg A.EAX }
      ]
    ]
    |> List.concat
  | T.If { cond; lt; lf } ->
    let sz, cmp, e1, e2 =
      match cond with
      | LCond cond -> A.L, cond.cmp, cond.p1, cond.p2
      | SCond cond -> A.L, cond.cmp, cond.p1, cond.p2
    in
    let jmptyp = if_cond_to_rev_jump_t cmp in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    [ munch_exp t1 e1 ~mfl
    ; munch_exp t2 e2 ~mfl
    ; [ A.Cmp { lhs = t1; size = sz; rhs = t2 }
      ; A.Cjmp { typ = jmptyp; l = lf }
      ; A.Jmp lt
      ]
    ]
    |> List.concat
  | T.MovToMem { mem; src } ->
    let t1 = A.Temp (Temp.create ()) in
    [ munch_exp t1 src ~mfl
    ; [ A.MovTo { dest = A.Temp mem; size = munch_size (T.size src); src = t1 } ]
    ]
    |> List.concat
  | T.MovFuncApp { dest; fname; args } ->
    let cogen_arg e =
      let t = Temp.create () in
      let sz = munch_size (T.size e) in
      (t, sz), munch_exp (A.Temp t) e ~mfl
    in
    let ops, cogen_args = List.map args ~f:cogen_arg |> List.unzip in
    let cogen_args = List.concat cogen_args in
    let args_mv_f i (t, sz) =
      if i < 6
      then Some (A.Mov { dest = A.Reg (arg_i_to_reg i); src = A.Temp t; size = sz })
      else None
    in
    let args_mv = List.mapi ops ~f:args_mv_f |> List.filter_opt in
    let args_in_regs_f i (_, sz) = if i < 6 then Some (arg_i_to_reg i, sz) else None in
    let args_in_regs = List.filter_mapi ops ~f:args_in_regs_f in
    let args_overflow_f i op = if i >= 6 then Some op else None in
    let args_overflow = List.filter_mapi ops ~f:args_overflow_f in
    let call = A.Call { fname; args_in_regs; args_overflow } in
    (match dest with
     | None -> [ cogen_args; args_mv; [ call ] ] |> List.concat
     | Some (d, sz) ->
       [ cogen_args
       ; args_mv
       ; [ call; A.Mov { dest = A.Temp d; size = munch_size sz; src = A.Reg A.EAX } ]
       ]
       |> List.concat)
;;

let munch_block ({ label; block; jump } : T.block) ~(mfl : Label.t) : A.block =
  let jump =
    match jump with
    | T.JRet -> A.JRet
    | T.JCon { lt; lf } -> A.JCon { jt = lt; jf = lf }
    | T.JUncon l -> A.JUncon l
  in
  let block' = List.map ~f:(fun s -> munch_stm s ~mfl) block |> List.concat in
  { label; block = block'; jump }
;;

let codegen (prog : T.program) ~(mfl : Label.t) : A.program =
  let map_f ({ fname; args; fdef } : T.fspace_block) : A.fspace =
    let args = List.map args ~f:(fun (t, i) -> t, munch_size i) in
    let fdef_blocks = List.map fdef ~f:(fun b -> munch_block b ~mfl) in
    { fname; args; fdef_blocks }
  in
  List.map prog ~f:map_f
;;
