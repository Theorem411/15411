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

(*_ sort the binops into categories for easier typechecking *)
type intop = Plus | Minus | Times
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
type ebop = 
  | IntOp of eintop
let binop_efkt_ebop = function 
  | A.Divided_by -> IntOp Div
  | A.Modulo -> IntOp Mod

module StatSemanticExpr = 
  struct
    type hyps = {
      ctx : gamma
    ; init : delta
    ; exp : A.mexp
    }
    let rec typechecker hyps typ = 
      let typ' = typesynther hyps in
        if T.equal typ typ' then ()
        else error ~msg:(sprintf "expression is does not type check: claim type `%s` but actually has type `%s`" (T._tostring typ) (T._tostring typ')) ~ast:hyps.exp
    and typesynther hyps = 
      let old_ctx = hyps.ctx in
      let old_init = hyps.init in
      match Mark.data hyps.exp with 
      | A.Var t -> 
        (if SS.mem hyps.init t then 
            match SM.find hyps.ctx t with 
            | Some typ -> typ 
            | None -> error ~msg:(sprintf "variable `%s` is not declared when first used" (Symbol.name t)) ~ast:hyps.exp
        else error ~msg:(sprintf "variable `%s` is not initialized when first used" (Symbol.name t)) ~ast:hyps.exp)
      | A.Const _ -> T.Int
      | A.PureBinop bop -> (
        let hyps_lhs = {ctx=old_ctx; init=old_init; exp=bop.lhs} in
        let hyps_rhs = {ctx=old_ctx; init=old_init; exp=bop.rhs} in
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
        let hyps_lhs = {ctx=old_ctx; init=old_init; exp=eop.lhs} in
        let hyps_rhs = {ctx=old_ctx; init=old_init; exp=eop.rhs} in
        match binop_efkt_ebop eop.op with 
        | IntOp _ -> typechecker hyps_lhs T.Int; typechecker hyps_rhs T.Int; T.Int)
      | A.Unop uop -> (
        let hyps' = {ctx=old_ctx; init=old_init; exp=uop.operand} in
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
  }
  type inferTo = {
    typ : T.t
  ; init_new : delta
  }
  let hyps_init prog = {ctx=SM.empty; init=SS.empty; prog=prog}
  ;;
  let inferTo_init typ = {typ=typ; init_new=SS.empty}
  ;;
(*_ no need for typesynther at stmt level because the outmost type is known *)
  let rec semantic_checker hyps inferTo = 
    let old_ctx = hyps.ctx in
    let old_init = hyps.init in
    let typ = inferTo.typ in
    match Mark.data hyps.prog with 
    | A.Nop -> ()
    | _ -> raise TypeError
  ;;
end
;;

(*_ the topmost type must be Int because 
   int main () {.. } 
  *)
let static_semantic (prog : A.program) = 
    StatSemanticCmd.semantic_checker (StatSemanticCmd.hyps_init prog) (StatSemanticCmd.inferTo_init T.Int)
;;