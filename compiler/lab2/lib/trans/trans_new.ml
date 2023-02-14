(* open Core *)
module T = Tree_new
module A = Aste
module S = Symbol.Map

exception UnImplemented
exception TypeCheckFailure

let global_err = Error_msg.create ()
;;
let error ~msg ~ast = 
  Error_msg.error global_err (Mark.src_span ast) ~msg;
  raise TypeCheckFailure
;;

type tr_e_t = T.stm list * T.pexp
let rec translate_expr_rev (env : Temp.t S.t) (exp : A.mexp) : tr_e_t = 
  match Mark.data exp with 
  | A.Var id -> ([], T.Temp (S.find_exn env id))
  | A.Const n -> ([], T.Const n)
  | A.PureBinop pbop -> 
    let (cmd1, exp1) = translate_expr_rev env pbop.lhs in
    let (cmd2, exp2) = translate_expr_rev env pbop.rhs in
    let op_tr = T.aste_pure_binop_to_pbop pbop.op in
      (cmd2 @ cmd1, T.Pbop {op=op_tr; lhs=exp1; rhs=exp2})
  | A.EfktBinop ebop -> 
    let (cmd1, exp1) = translate_expr_rev env ebop.lhs in
    let (cmd2, exp2) = translate_expr_rev env ebop.rhs in
    let op_tr = T.aste_efkt_binop_to_ebop ebop.op in
    let f_new = Temp.create () in
      ( T.MovEfktExp {dest=f_new; ebop=op_tr; lhs=exp1; rhs=exp2} :: (cmd2 @ cmd1)
      , T.Temp f_new)
  | _ -> raise UnImplemented (*_ reserve boolean translation for later *)
and translate_expr (env : Temp.t S.t) (exp : Aste.mexp) : tr_e_t = 
  let (stm_lst, pexp) = translate_expr_rev env exp in
    List.rev stm_lst, pexp
and cp_rev (env : Temp.t S.t) (exp : A.mexp) (l : Label.t) (l' : Label.t) = 
  match Mark.data exp with
  | A.True -> [T.Goto l']
  | A.False -> [T.Goto l]
  | A.PureBinop bop -> (
      match bop.op with 
      | A.And -> []
      | A.Or -> []
      | A.Eq | A.Neq | A.Leq | A.Less | A.Geq | A.Greater -> []
      | _ -> raise UnImplemented
  )
  | A.EfktBinop bop -> raise UnImplemented
  | A.Unop uop -> (
      match uop.op with 
      (* | A.Not -> [] *)
      | _ -> failwith "Check this out."
  )
  | _ -> raise UnImplemented
and cp (env : Temp.t S.t) (exp : A.mexp) (l : Label.t) (l' : Label.t) =
  List.rev (cp_rev env exp l l')
;;