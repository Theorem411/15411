open Core
module T = Ctype
module SM = Symbol.Map
module SS = Symbol.Set
module A = Aste
(*_ exception and error printing *)
let global_err = Error_msg.create ()
;;
exception TypeError
let error ~msg ~ast = 
  Error_msg.error global_err (Mark.src_span ast) ~msg;
  raise TypeError
;;

(*_ gamma : set of variables (S.t) that are declared with type 
    delta : set of variables that are initialized *)
type gamma = T.t SM.t
type delta = SS.t

let ctx_find (ctx:gamma) sym ~ast = 
  match SM.find ctx sym with 
  | Some typ -> typ 
  | None -> 
    error ~msg:(sprintf "variable `%s` is not declared when first used" (Symbol.name sym)) ~ast:ast
;;
let ctx_find_and_check ctx sym typ ~ast = 
  let typ' = ctx_find ctx sym ~ast:ast in
    if (T.equal typ typ') then ()
    else 
      error ~msg:(sprintf "expression does not type check: claim type `%s` but actually has type `%s`" (T._tostring typ) (T._tostring typ')) ~ast:ast
;;
(*_ sort the binops into categories for easier typechecking *)
type intop = Plus | Minus | Times | BitAnd | BitOr | BitXor
type logop = And | Or
type cpop = Less | Leq | Greater | Geq
type polyop = Eq | Neq

type pbop = 
  | IntOp of intop 
  | LogOp of logop 
  | CpOp of cpop 
  | PolyOp of polyop

let binop_pure_pbop = function 
  | A.And -> LogOp And
  | A.Or -> LogOp Or
  | A.BitAnd -> IntOp BitAnd
  | A.BitOr -> IntOp BitOr
  | A.BitXor -> IntOp BitXor
  | A.Plus -> IntOp Plus
  | A.Minus -> IntOp Minus
  | A.Times -> IntOp Times
  | A.Leq -> CpOp Leq
  | A.Less -> CpOp Less
  | A.Geq -> CpOp Geq
  | A.Greater -> CpOp Greater
  | A.Eq -> PolyOp Eq
  | A.Neq -> PolyOp Neq
;;

type eintop = 
  | Div
  | Mod
  | ShftL
  | ShftR
type ebop = 
  | IntOp of eintop
let binop_efkt_ebop = function 
  | A.Divided_by -> IntOp Div
  | A.Modulo -> IntOp Mod
  | A.ShiftL -> IntOp ShftL
  | A.ShiftR -> IntOp ShftR

module StatSemanticExpr = 
  struct
    type hyps = {
      ctx : gamma
    ; init : delta
    ; exp : A.mexp
    }
    let hyps_create ~ctx ~init ~exp = {ctx=ctx; init=init; exp=exp}
    let rec typechecker hyps typ = 
      let typ' = typesynther hyps in
        if T.equal typ typ' then ()
        else 
          error ~msg:(sprintf "expression is does not type check: claim type `%s` but actually has type `%s`" (T._tostring typ) (T._tostring typ')) ~ast:hyps.exp
    and typesynther hyps = 
      match Mark.data hyps.exp with 
      | A.True -> T.Bool
      | A.False -> T.Bool
      | A.Var t -> 
        (if SS.mem hyps.init t then ctx_find hyps.ctx t ~ast:hyps.exp
        else 
          error ~msg:(sprintf "variable `%s` is not initialized when first used" (Symbol.name t)) ~ast:hyps.exp)
      | A.Const _ -> T.Int
      | A.Ternary tern -> 
        let hyps_l = { hyps with exp=tern.lb } in
        let typ = typesynther hyps_l in
        let hyps_r = { hyps with exp=tern.rb } in
          typechecker hyps_r typ; T.Bool
      | A.PureBinop bop -> (
        let hyps_lhs = { hyps with exp=bop.lhs}
        and hyps_rhs = { hyps with exp=bop.rhs} in
        match binop_pure_pbop bop.op with 
        | IntOp _ -> typechecker hyps_lhs T.Int; typechecker hyps_rhs T.Int; T.Int
        | LogOp _ -> typechecker hyps_lhs T.Bool; typechecker hyps_rhs T.Bool; T.Bool
        | CpOp _ -> typechecker hyps_lhs T.Int; typechecker hyps_rhs T.Int; T.Bool
        | PolyOp _ -> 
          let 
            typ = typesynther hyps_lhs 
          in 
            typechecker hyps_rhs typ; T.Bool
        )
      | A.EfktBinop eop -> (
        let hyps_lhs = { hyps with exp=eop.lhs} 
        and hyps_rhs = { hyps with exp=eop.rhs} in
        match binop_efkt_ebop eop.op with 
        | IntOp _ -> typechecker hyps_lhs T.Int; typechecker hyps_rhs T.Int; T.Int)
      | A.Unop uop -> (
        let hyps' = { hyps with exp=uop.operand} in
          typechecker hyps' T.Bool; T.Bool
      )
    ;;
  end
;;
module StatSemanticCmd = 
struct 
  type hyps = {
    ctx : gamma
  ; init : delta
  ; prog : A.program
  ; typ : T.t
  }
  let hyps_init ~prog ~typ = {ctx=SM.empty; init=SS.empty; prog=prog; typ=typ}
  ;;
  let rec semantic_synther hyps = 
    match Mark.data hyps.prog with 
    | A.Nop -> hyps.init
    | A.Seq (prog1, prog2) -> 
      let hyps' = { hyps with prog=prog1 } in
      let init' = semantic_synther hyps' in
      let hyps'' = { hyps with prog=prog2; init=init'} in
      let init'' = semantic_synther hyps'' in
        init''
    | A.Return exp -> 
      let dom = SM.key_set hyps.ctx in
      let hyps_expr = StatSemanticExpr.hyps_create ~ctx:hyps.ctx ~init:hyps.init ~exp:exp in
        StatSemanticExpr.typechecker hyps_expr hyps.typ
      ; dom
    | A.Assign assn -> 
      let hyps_expr = StatSemanticExpr.hyps_create ~ctx:hyps.ctx ~init:hyps.init ~exp:assn.exp in
      let () = ctx_find_and_check hyps.ctx assn.var hyps.typ ~ast:hyps.prog in
      let init' = SS.add hyps.init assn.var in
        StatSemanticExpr.typechecker hyps_expr hyps.typ
      ; init'
    | A.Declare decl -> 
      let ctx' = match SM.add hyps.ctx ~key:decl.var ~data:decl.typ with 
        | `Ok c -> c 
        | `Duplicate -> 
          error ~msg:(sprintf "Same variable %s should not be declared twice" (Symbol.name decl.var)) ~ast:hyps.prog
      in
      let init' = semantic_synther { hyps with prog=decl.body; ctx=ctx'} in
        SS.remove init' decl.var
    | A.If ifs -> 
      let hyps_expr = StatSemanticExpr.hyps_create ~ctx:hyps.ctx ~init:hyps.init ~exp:ifs.cond in
      let init1 = semantic_synther { hyps with prog=ifs.lb } in
      let init2 = semantic_synther { hyps with prog=ifs.rb } in
        StatSemanticExpr.typechecker hyps_expr T.Bool
      ; SS.inter init1 init2
    | A.While loop -> 
      let hyps_expr = StatSemanticExpr.hyps_create ~ctx:hyps.ctx ~init:hyps.init ~exp:loop.cond in
      let _ : delta = semantic_synther { hyps with prog=loop.body } in
        StatSemanticExpr.typechecker hyps_expr T.Bool
      ; hyps.init
    | A.NakedExpr exp -> 
      let hyps_expr = StatSemanticExpr.hyps_create ~ctx:hyps.ctx ~init:hyps.init ~exp:exp in
        StatSemanticExpr.typechecker hyps_expr hyps.typ
      ; hyps.init (*_ not defined in lecture notes! Be careful of BUG *)
end
;;

(*_ the topmost type must be Int because 
   int main () {.. } 
  *)
let static_semantic (prog : A.program) : unit = 
  let _:delta = StatSemanticCmd.semantic_synther (StatSemanticCmd.hyps_init ~prog:prog ~typ:T.Int) in
    ()
;;