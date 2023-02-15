open Core
module T = Tree_new
module A = Aste
module S = Symbol.Map

exception UnImplemented
exception TypeCheckFailure
exception BinopConvertFailure
let global_err = Error_msg.create ()
;;
let error ~msg ~ast = 
  Error_msg.error global_err (Mark.src_span ast) ~msg;
  raise TypeCheckFailure
;;

let aste_binop_to_tree_pbop_exn = function 
  | A.Plus -> T.Add
  | A.Minus -> T.Sub
  | A.Times -> T.Mul
  | A.BitAnd -> T.BitAnd
  | A.BitOr -> T.BitOr
  | A.BitXor -> T.BitXor
  | _ -> raise BinopConvertFailure

let aste_binop_to_tree_cbop_exn = function 
  | A.Eq -> T.Eq
  | A.Neq -> T.Neq
  | A.Leq -> T.Leq
  | A.Less -> T.Less
  | A.Geq -> T.Geq
  | A.Greater -> T.Greater
  | _ -> raise BinopConvertFailure

let aste_binop_to_tree_ebop_exn = function
  | A.Divided_by -> T.Div
  | A.Modulo -> T.Mod
  | A.ShiftL -> T.ShftL
  | A.ShiftR -> T.ShftR


(*_ tr (e) = <e_up, e_down>, 
   - e_up = cmd's generated 
   - e_down = pure expr e evaluated to *)
type tr_e_t = T.stm list * T.pexp
(* let rec translate_expr_rev (env : Temp.t S.t) (exp : A.mexp) : tr_e_t = 
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

*)

let translate_exp (env : Temp.t S.t) (exp : Aste.mexp) : tr_e_t = 
  let rec tr_exp_rev (env : Temp.t S.t) (exp : A.mexp) : tr_e_t = 
    match Mark.data exp with 
    | A.Var id -> [], T.Temp (S.find_exn env id)
    | A.Const n -> [], T.Const n
    | A.PureBinop bop -> tr_pure_binop_rev env bop.op bop.lhs bop.rhs
    | _ -> raise UnImplemented
  and tr_pure_binop_rev (env : Temp.t S.t) (op : A.binop_pure) (e1 : A.mexp) (e2 : A.mexp) = 
    match op with
    | A.Plus | A.Minus | A.Times -> 
      let op' = aste_binop_to_tree_pbop_exn op in
      let (e1c, e1p) = tr_exp_rev env e1 in
      let (e2c, e2p) = tr_exp_rev env e2 in
        e2c @ e1c, T.Pbop { op = op'; lhs = e1p; rhs = e2p }
    | A.BitAnd | A.BitOr | A.BitXor -> raise UnImplemented 
    | _ -> raise UnImplemented
  in 
  let (stm_lst, pexp) = tr_exp_rev env exp in
    List.rev stm_lst, pexp
and cp (env : Temp.t S.t) (exp : A.mexp) (l : Label.t) (l' : Label.t) = 
  let rec cp_rev (env : Temp.t S.t) (exp : A.mexp) (l : Label.t) (l' : Label.t) =
    match Mark.data exp with 
    | A.Const n -> 
      if (Int32.equal n (Int32.of_int_exn 0)) then [T.Goto l']
      else if (Int32.equal n (Int32.of_int_exn 1)) then [T.Goto l]
      else 
        let err_msg = sprintf "cp should not handle const other than 0 and 1" in
        error ~msg:err_msg ~ast:exp
    | A.Unop e -> 
        (match e.op with 
        | A.L_not -> cp_rev env exp l' l
        | A.B_not -> 
          let err_msg = sprintf "cp should not handle logical not (!)" in
          error ~msg:err_msg ~ast:exp)
    | _ -> raise UnImplemented 
  in
    List.rev (cp_rev env exp l l')
;;