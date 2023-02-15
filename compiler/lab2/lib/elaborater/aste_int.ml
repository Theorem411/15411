open Core
module A = Aste
module S = Symbol

let rec booster_elaborate_mexp (e : A.mexp) : A.mexp =
  let src_span = Mark.src_span e in
  let mark = fun exp -> Mark.mark' exp src_span in
  match Mark.data e with 
  | A.Var _ -> e
  | A.Const _ -> e
  | A.True -> A.Const (Int32.of_int_exn 1) |> mark
  | A.False -> A.Const (Int32.of_int_exn 0) |> mark
  | A.PureBinop pbop -> 
    let lhs' = booster_elaborate_mexp pbop.lhs in
    let rhs' = booster_elaborate_mexp pbop.rhs in
    let pbop' = (
      match pbop.op with 
      | A.And -> A.BitAnd
      | A.Or -> A.BitOr
      | _ -> pbop.op) 
    in
      A.PureBinop { op = pbop'; lhs = lhs'; rhs = rhs' } |> mark
  | A.EfktBinop ebop -> 
    let lhs' = booster_elaborate_mexp ebop.lhs in
    let rhs' = booster_elaborate_mexp ebop.rhs in
      A.EfktBinop { ebop with lhs = lhs'; rhs = rhs' } |> mark
  | A.Unop uop -> 
    let operand' = booster_elaborate_mexp uop.operand in
    let const_minus_1 = A.Const (Int32.of_int_exn (-1)) |> mark in
    (match uop.op with 
    | A.B_not -> A.PureBinop { op = A.BitXor; lhs = operand' ; rhs = const_minus_1 }
    | A.L_not -> A.Unop { uop with operand = operand' }
    ) |> mark
  | A.Ternary tern -> 
    let cond' = booster_elaborate_mexp tern.cond in
    let lb' = booster_elaborate_mexp tern.lb in 
    let rb' = booster_elaborate_mexp tern.rb in
      A.Ternary { cond = cond'; lb = lb'; rb = rb' } |> mark
;;
    
let rec booster_elaborate (prog : A.program) : A.program = 
  let src_span = Mark.src_span prog in
  let mark = fun stm -> Mark.mark' stm src_span in
  let stm = (
    match Mark.data prog with 
    | A.Declare decl -> A.Declare { decl with body = booster_elaborate decl.body}
    | A.Assign assn -> A.Assign { assn with exp = booster_elaborate_mexp assn.exp }
    | A.If ifs -> 
      A.If { cond = booster_elaborate_mexp ifs.cond
           ; lb = booster_elaborate ifs.lb
           ; rb = booster_elaborate ifs.rb }
    | A.While loop -> 
      A.While { cond = booster_elaborate_mexp loop.cond
              ; body = booster_elaborate loop.body }
    | A.Return e -> A.Return (booster_elaborate_mexp e)
    | A.Nop -> A.Nop
    | A.Seq (s1, s2) -> A.Seq (booster_elaborate s1, booster_elaborate s2)
    | A.NakedExpr e -> A.NakedExpr (booster_elaborate_mexp e) 
    )
  in
    mark stm
;;