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

let lea_off_ref = ref false
let set_lea_off b = lea_off_ref := b
let lea_on () = not !lea_off_ref

let is_lea_scale = function
  | 1 | 2 | 4 | 8 -> lea_on () (* true only if lea_on *)
  | _ -> false
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
  | _ -> A.L
;;

let munch_exp ~(unsafe : bool) (dest : A.operand) (exp : T.mpexp) ~(mfl : Label.t)
    : A.instr list
  =
  let rec munch_exp_rev (dest : A.operand) (exp : T.mpexp) ~(mfl : Label.t) : A.instr list
    =
    let e, esize = exp in
    match e with
    | T.Const n -> [ A.Mov { dest; size = A.S; src = A.Imm (Int64.of_int32_exn n) } ]
    | T.Temp t -> [ A.Mov { dest; size = munch_size esize; src = A.Temp t } ]
    | T.Binop { op; lhs; rhs } ->
      let op = munch_binary_op op in
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      A.PureBinop { op; dest; size = A.S; lhs = t1; rhs = t2 }
      :: (munch_exp_rev ~mfl t2 rhs @ munch_exp_rev ~mfl t1 lhs)
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
      let src = A.Temp (Temp.create ()) in
      A.Unop { op; dest; src } :: munch_exp_rev ~mfl src p
    | T.Mem T.Null -> if unsafe then [] else [ A.Jmp mfl ]
    | T.Mem (T.Ptr { start; off }) ->
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let zero8 = A.Imm (Int64.of_int_exn 0) in
      let off8 = A.Imm (Int64.of_int_exn off) in
      let size = munch_size esize in
      let chck =
        if unsafe
        then
          [ A.MovFrom { dest; src = t2; size }
          ; A.PureBinop { dest = t2; size = A.L; lhs = t1; op = A.Add; rhs = off8 }
          ]
        else
          [ A.MovFrom { dest; src = t2; size }
          ; A.PureBinop { dest = t2; size = A.L; lhs = t1; op = A.Add; rhs = off8 }
          ; A.Cjmp { typ = A.Je; l = mfl }
          ; A.Cmp { lhs = t1; rhs = zero8; size = A.L }
          ]
      in
      chck @ munch_exp_rev ~mfl t1 start
    | T.Mem (T.Arr { head; idx; typ_size; extra }) ->
      let a = A.Temp (Temp.create ()) in
      let b = A.Temp (Temp.create ()) in
      let checks =
        if unsafe
        then []
        else (
          let zero8 = A.Imm (Int64.of_int_exn 0) in
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
          let eight8 = A.Imm (Int64.of_int_exn 8) in
          let chk_idx_len_rev =
            [ A.PureBinop { op = A.Sub; size = A.L; dest = t3; lhs = a; rhs = eight8 }
            ; A.MovFrom { dest = t4; size = A.S; src = t3 }
            ; A.Cmp { size = A.S; lhs = b; rhs = t4 }
            ; A.Cjmp { typ = A.Jge; l = mfl }
            ]
            |> List.rev
          in
          [ chk_idx_len_rev; chk_idx_pos_rev; chk_head_null_rev ] |> List.concat)
      in
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let res_rev =
        if is_lea_scale typ_size
        then
          [ A.LeaArray
              { dest = t1; base = a; index = b; scale = typ_size; offset = extra }
          ; A.MovFrom { dest; size = munch_size esize; src = t1 }
          ]
          |> List.rev
        else
          [ A.Mov { dest = t1; size = A.L; src = a }
          ; A.MovSxd { dest = b; src = b }
          ; A.PureBinop
              { op = A.Mul
              ; size = A.L
              ; dest = t2
              ; lhs = b
              ; rhs = A.Imm (Int64.of_int_exn typ_size)
              }
          ; A.PureBinop { op = A.Add; size = A.L; dest = t1; lhs = t1; rhs = t2 }
          ; A.PureBinop
              { op = A.Add
              ; size = A.L
              ; dest = t1
              ; lhs = t1
              ; rhs = A.Imm (Int64.of_int_exn extra)
              }
          ; A.MovFrom { dest; size = munch_size esize; src = t1 }
          ]
          |> List.rev
      in
      (*_ view in reverse order *)
      [ res_rev; checks; munch_exp_rev ~mfl b idx; munch_exp_rev ~mfl a head ]
      |> List.concat
    | T.Addr T.Null -> [ A.Mov { dest; size = A.L; src = A.Imm (Int64.of_int_exn 0) } ]
    | T.Addr (T.Ptr { start; off }) ->
      let t = A.Temp (Temp.create ()) in
      A.PureBinop
        { op = A.Add; size = A.L; dest; lhs = t; rhs = A.Imm (Int64.of_int_exn off) }
      :: munch_exp_rev ~mfl t start
    | T.Addr (T.Arr { head; idx; typ_size; extra }) ->
      let t1 = A.Temp (Temp.create ()) in
      let t2 = A.Temp (Temp.create ()) in
      let a = A.Temp (Temp.create ()) in
      let b = A.Temp (Temp.create ()) in
      let res =
        if is_lea_scale typ_size
        then
          [ A.LeaArray
              { dest = t1; base = a; index = b; scale = typ_size; offset = extra }
          ]
          |> List.rev
        else
          [ A.MovSxd { dest = b; src = b }
          ; A.PureBinop
              { dest = t1
              ; size = A.L
              ; lhs = b
              ; op = A.Mul
              ; rhs = A.Imm (Int64.of_int_exn typ_size)
              }
          ; A.Mov { dest = t2; size = A.L; src = a }
          ; A.PureBinop { dest = t2; size = A.L; lhs = t2; op = A.Add; rhs = t1 }
          ; A.PureBinop
              { dest = t2
              ; size = A.L
              ; lhs = t2
              ; op = A.Add
              ; rhs = A.Imm (Int64.of_int_exn extra)
              }
          ; A.Mov { dest; size = A.L; src = t2 }
          ]
          |> List.rev
      in
      [ res; munch_exp_rev ~mfl b idx; munch_exp_rev ~mfl a head ] |> List.concat
  in
  List.rev (munch_exp_rev dest exp ~mfl)
;;

let munch_stm (stm : T.stm) ~(mfl : Label.t) ~(unsafe : bool) : A.instr list =
  match stm with
  | T.MovPureExp { dest; src } -> munch_exp ~unsafe (A.Temp dest) src ~mfl
  | T.MovEfktExp { dest; ebop; lhs; rhs } ->
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    [ munch_exp ~unsafe t1 lhs ~mfl
    ; munch_exp ~unsafe t2 rhs ~mfl
    ; [ A.EfktBinop { dest = A.Temp dest; op = munch_efkt_op ebop; lhs = t1; rhs = t2 } ]
    ]
    |> List.concat
  | T.Goto label -> [ A.Jmp label ]
  | T.Label label -> [ A.Lab label ]
  | T.Return eopt ->
    (* return e is implemented as %eax <- e *)
    (match eopt with
    | None -> [ A.Ret ]
    | Some e -> munch_exp ~unsafe (A.Reg A.EAX) e ~mfl @ [ A.Ret ])
  | T.AssertFail -> [ A.AssertFail ]
  | T.Alloc { dest; size } ->
    [ (* edi <-4 size *)
      A.Mov { dest = A.Reg A.EDI; size = A.S; src = A.Imm (Int64.of_int_exn size) }
      (* call allocjavaway *)
    ; A.Call
        { fname = Symbol.symbol (CustomAssembly.alloc_fname ~unsafe)
        ; args_overflow = []
        ; args_in_regs = [ A.EDI, A.S ]
        ; tail_call = false
        }
      (* dest <-8 rax *)
    ; A.Mov { dest = A.Temp dest; size = A.L; src = A.Reg A.EAX }
    ]
  | T.Calloc { dest; len; typ } ->
    let t1 = A.Temp (Temp.create ()) in
    [ munch_exp ~unsafe t1 len ~mfl
    ; [ (* edi <-4 typ *)
        A.Mov { dest = A.Reg A.EDI; size = A.S; src = A.Imm (Int64.of_int_exn typ) }
        (* edi <-4 len *)
      ; A.Mov { dest = A.Reg A.ESI; size = A.S; src = t1 } (* call allocjavaway *)
      ; A.Call
          { fname = Symbol.symbol (CustomAssembly.alloc_array_fname ~unsafe)
          ; args_overflow = []
          ; args_in_regs = [ A.EDI, A.S; A.ESI, A.S ]
          ; tail_call = false
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
      | SCond cond -> A.S, cond.cmp, cond.p1, cond.p2
    in
    let jmptyp = if_cond_to_rev_jump_t cmp in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    [ munch_exp ~unsafe t1 e1 ~mfl
    ; munch_exp ~unsafe t2 e2 ~mfl
    ; [ A.Cmp { lhs = t1; size = sz; rhs = t2 }
      ; A.Cjmp { typ = jmptyp; l = lf }
      ; A.Jmp lt
      ]
    ]
    |> List.concat
  | T.MovToMem { addr = T.Null; _ } -> if unsafe then [] else [ A.Jmp mfl ]
  | T.MovToMem { addr = T.Ptr { start; off }; src } ->
    let ts = A.Temp (Temp.create ()) in
    let codegen_start = munch_exp ~unsafe ts start ~mfl in
    let nullchk =
      if unsafe
      then []
      else
        [ A.Cmp { lhs = ts; size = A.L; rhs = A.Imm (Int64.of_int_exn 0) }
        ; A.Cjmp { typ = A.Je; l = mfl }
        ]
    in
    let t = A.Temp (Temp.create ()) in
    let movp =
      [ A.PureBinop
          { dest = t
          ; size = A.L
          ; lhs = ts
          ; op = A.Add
          ; rhs = A.Imm (Int64.of_int_exn off)
          }
      ]
    in
    let tr = A.Temp (Temp.create ()) in
    let codegen_src = munch_exp ~unsafe tr src ~mfl in
    let movr = [ A.MovTo { dest = t; size = munch_size (T.size src); src = tr } ] in
    [ codegen_start; nullchk; movp; codegen_src; movr ] |> List.concat
  | T.MovToMem { addr = T.Arr { head; idx; typ_size; extra }; src } ->
    let th = A.Temp (Temp.create ()) in
    let ti = A.Temp (Temp.create ()) in
    let ti' = A.Temp (Temp.create ()) in
    let tm = A.Temp (Temp.create ()) in
    let tl = A.Temp (Temp.create ()) in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    let t = A.Temp (Temp.create ()) in
    let codegen_head = munch_exp ~unsafe th head ~mfl in
    let codegen_idx = munch_exp ~unsafe ti idx ~mfl in
    let adrs_calc =
      if unsafe
      then
        if is_lea_scale typ_size
        then
          [ A.LeaArray
              { dest = t; base = th; index = ti; scale = typ_size; offset = extra }
          ]
        else
          [ A.MovSxd { dest = ti'; src = ti }
          ; A.PureBinop
              { dest = t1
              ; size = A.L
              ; lhs = ti'
              ; op = A.Mul
              ; rhs = A.Imm (Int64.of_int_exn typ_size)
              }
          ; A.PureBinop
              { dest = t2
              ; size = A.L
              ; lhs = t1
              ; op = A.Add
              ; rhs = A.Imm (Int64.of_int_exn extra)
              }
          ; A.PureBinop { dest = t; size = A.L; lhs = th; op = A.Add; rhs = t2 }
          ]
      else
        [ A.Cmp { lhs = th; size = A.L; rhs = A.Imm (Int64.of_int_exn 0) }
        ; A.Cjmp { typ = A.Je; l = mfl }
        ; A.MovSxd { dest = ti'; src = ti }
        ; A.Cmp { lhs = ti; size = A.S; rhs = A.Imm (Int64.of_int_exn 0) }
        ; A.Cjmp { typ = A.Jl; l = mfl }
        ; A.PureBinop
            { dest = tm
            ; size = A.L
            ; lhs = th
            ; op = A.Sub
            ; rhs = A.Imm (Int64.of_int_exn 8)
            }
        ; A.MovFrom { dest = tl; size = A.S; src = tm }
        ; A.Cmp { lhs = ti; size = A.S; rhs = tl }
        ; A.Cjmp { typ = A.Jge; l = mfl }
        ; A.PureBinop
            { dest = t1
            ; size = A.L
            ; lhs = ti'
            ; op = A.Mul
            ; rhs = A.Imm (Int64.of_int_exn typ_size)
            }
        ; A.PureBinop
            { dest = t2
            ; size = A.L
            ; lhs = t1
            ; op = A.Add
            ; rhs = A.Imm (Int64.of_int_exn extra)
            }
        ; A.PureBinop { dest = t; size = A.L; lhs = th; op = A.Add; rhs = t2 }
        ]
    in
    let tr = A.Temp (Temp.create ()) in
    let codegen_src = munch_exp ~unsafe tr src ~mfl in
    let mov = [ A.MovTo { dest = t; size = munch_size (T.size src); src = tr } ] in
    [ codegen_head; codegen_idx; adrs_calc; codegen_src; mov ] |> List.concat
  | T.MovFuncApp { dest; fname; args; tail_call } ->
    let cogen_arg ~unsafe e =
      let t = Temp.create () in
      let sz = munch_size (T.size e) in
      (t, sz), munch_exp ~unsafe (A.Temp t) e ~mfl
    in
    let ops, cogen_args = List.map args ~f:(cogen_arg ~unsafe) |> List.unzip in
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
    let call =
      A.Call
        { fname
        ; args_in_regs
        ; args_overflow = List.map ~f:(fun (t, sz) -> A.Temp t, sz) args_overflow
        ; tail_call
        }
    in
    (match dest with
    | None -> [ cogen_args; args_mv; [ call ] ] |> List.concat
    | Some (d, sz) ->
      [ cogen_args
      ; args_mv
      ; [ call; A.Mov { dest = A.Temp d; size = munch_size sz; src = A.Reg A.EAX } ]
      ]
      |> List.concat)
;;

let munch_arg_moves (args : (Temp.t * A.size) list) =
  let args, stacks = List.take args 6, List.drop args 6 in
  List.mapi args ~f:(fun i (t, sz) ->
      A.Mov { dest = A.Temp t; src = A.Reg (A.arg_i_to_reg i); size = sz })
  @ [ A.LoadFromStack stacks ]
;;

let munch_block
    ({ label; block; jump; loop_depth } : T.block)
    ~(mfl : Label.t)
    ~(unsafe : bool)
    ~(args : (Temp.t * A.size) list)
    : A.block
  =
  let jump =
    match jump with
    | T.JRet -> A.JRet
    | T.JCon { lt; lf } -> A.JCon { jt = lt; jf = lf }
    | T.JUncon l -> A.JUncon l
  in
  let block' = List.map ~f:(fun s -> munch_stm s ~mfl ~unsafe) block |> List.concat in
  let arg_moves =
    match label with
    | Label.BlockLbl _ -> []
    | Label.FunName _ -> munch_arg_moves args
  in
  { label; block = arg_moves @ block'; jump; depth = loop_depth }
;;

let codegen (prog : T.program) ~(mfl : Label.t) ~(unsafe : bool) : A.program =
  let map_f ~(unsafe : bool) ({ fname; args; fdef } : T.fspace_block) : A.fspace =
    let tmp_cnt_init = Temp.get_counter () in
    let args = List.map args ~f:(fun (t, i) -> t, munch_size i) in
    let fdef_blocks = List.map fdef ~f:(fun b -> munch_block b ~unsafe ~mfl ~args) in
    let tmp_cnt_final = Temp.get_counter () in
    { fname; args; fdef_blocks; tmp_cnt = tmp_cnt_final - tmp_cnt_init }
  in
  List.map prog ~f:(map_f ~unsafe)
;;
