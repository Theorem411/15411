open Core
module T = Tree_new
module A = Assem_new

(*_ *)
let if_cond_to_rev_jump_t (cond : T.cond) =
  match cond.cmp with
  | T.Leq -> A.Jg
  | T.Less -> A.Jge
  | T.Geq -> A.Jl
  | T.Greater -> A.Jle
  | T.Eq -> A.Jne
  | T.Neq -> A.Je
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

let munch_exp : A.operand -> T.pexp -> A.instr list =
  let rec munch_exp_rev (dest : A.operand) (exp : T.pexp) =
    match exp with
    | T.Const n -> [ A.Mov { dest; src = A.Imm n } ]
    | T.Temp t -> [ A.Mov { dest; src = A.Temp t } ]
    | T.Binop bop -> munch_binop dest (bop.op, bop.lhs, bop.rhs)
    | T.Cmpop cop -> munch_cmpop dest (cop.op, cop.lhs, cop.rhs)
    | T.Unop uop -> munch_unop dest (uop.op, uop.p)
  and munch_unop (dest : A.operand) ((uop, e) : T.unop * T.pexp) : A.instr list =
    let op = munch_unary_op uop in
    A.Unop { op; dest } :: munch_exp_rev dest e
  and munch_cmpop (dest : A.operand) ((cop, e1, e2) : T.cbop * T.pexp * T.pexp)
    : A.instr list
    =
    let typ = cmp_to_set_t cop in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    (*_ view in reverse order *)
    A.Set { typ; src = dest }
    :: A.Cmp (t1, t2)
    :: (munch_exp_rev t2 e2 @ munch_exp_rev t1 e1)
  and munch_binop (dest : A.operand) ((bop, e1, e2) : T.pbop * T.pexp * T.pexp)
    : A.instr list
    =
    let op = munch_binary_op bop in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    (*_ view in reverse order *)
    A.PureBinop { op; dest; lhs = t1; rhs = t2 }
    :: (munch_exp_rev t2 e2 @ munch_exp_rev t1 e1)
  in
  fun dest exp -> List.rev (munch_exp_rev dest exp)
;;

let munch_stm = function
  | T.MovPureExp mv -> munch_exp (A.Temp mv.dest) mv.src
  | T.MovEfktExp mv ->
    let op = munch_efkt_op mv.ebop in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    munch_exp t1 mv.lhs
    @ munch_exp t2 mv.rhs
    @ [ A.EfktBinop { op; dest = A.Temp mv.dest; lhs = t1; rhs = t2 } ]
  | T.If ifs ->
    let jmptyp = if_cond_to_rev_jump_t ifs.cond in
    let e1 = ifs.cond.p1 in
    let e2 = ifs.cond.p2 in
    let t1 = A.Temp (Temp.create ()) in
    let t2 = A.Temp (Temp.create ()) in
    munch_exp t1 e1
    @ munch_exp t2 e2
    @ [ A.Cmp (t1, t2); A.Cjmp { typ = jmptyp; l = ifs.lf } ]
  | T.Goto l -> [ A.Jmp l ]
  | T.Label l -> [ A.Lab l ]
  | T.Return e ->
    (* return e is implemented as %eax <- e *)
    munch_exp (A.Reg A.EAX) e
;;

let cogen inp = List.concat_map ~f:munch_stm inp