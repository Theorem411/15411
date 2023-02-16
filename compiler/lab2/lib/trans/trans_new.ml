open Core
module T = Tree_new
module A = Aste
module S = Symbol.Map
(* 
exception TypeCheckFailure

let global_err = Error_msg.create ()

let error ~msg ~ast =
  Error_msg.error global_err (Mark.src_span ast) ~msg;
  raise TypeCheckFailure
;; *)

let aste_binop_to_tree_pbop = function
  | A.Plus -> T.Add
  | A.Minus -> T.Sub
  | A.Times -> T.Mul
  | A.BitAnd -> T.BitAnd
  | A.BitOr -> T.BitOr
  | A.BitXor -> T.BitXor
;;

let aste_binop_to_tree_cbop = function
  | A.Eq -> T.Eq
  | A.Neq -> T.Neq
  | A.Leq -> T.Leq
  | A.Less -> T.Less
  | A.Geq -> T.Geq
  | A.Greater -> T.Greater
;;

let aste_binop_to_tree_ebop = function
  | A.Divided_by -> T.Div
  | A.Modulo -> T.Mod
  | A.ShiftL -> T.ShftL
  | A.ShiftR -> T.ShftR
;;

(*_ tr (e) = <e_up, e_down>, 
   - e_cmd = cmd's generated 
   - e_exp = pure expr e evaluated to *)
type tr_exp_t = T.stm list * T.pexp

(**_ #1 BUG source in this file: 
    - all _rev : recursive call has the wrong order 
*)
let rec tr_exp_rev (env : Temp.t S.t) (exp : A.mexp) : tr_exp_t =
  match Mark.data exp with
  | A.Var id -> [], T.Temp (S.find_exn env id)
  | A.Const n -> [], T.Const n
  | A.True -> [], T.Const (Int32.of_int_exn 1)
  | A.False -> [], T.Const (Int32.of_int_exn 0)
  | A.PureBinop bop ->
    let op' = aste_binop_to_tree_pbop bop.op in
    let cmd1, exp1 = tr_exp_rev env bop.lhs in
    let cmd2, exp2 = tr_exp_rev env bop.rhs in
    cmd2 @ cmd1, T.Binop { op = op'; lhs = exp1; rhs = exp2 }
  | A.EfktBinop eop ->
    let op' = aste_binop_to_tree_ebop eop.op in
    let cmd1, exp1 = tr_exp_rev env eop.lhs in
    let cmd2, exp2 = tr_exp_rev env eop.rhs in
    let t = Temp.create () in
    ( T.MovEfktExp { dest = t; ebop = op'; lhs = exp1; rhs = exp2 } :: (cmd2 @ cmd1)
    , T.Temp t )
  | A.CmpBinop cop ->
    let op' = aste_binop_to_tree_cbop cop.op in
    let cmd1, exp1 = tr_exp_rev env cop.lhs in
    let cmd2, exp2 = tr_exp_rev env cop.rhs in
    cmd2 @ cmd1, T.Cmpop { op = op'; lhs = exp1; rhs = exp2 }
  | A.Ternary tern ->
    let e1, e2, e3 = tern.cond, tern.lb, tern.rb in
    tr_exp_ternary env e1 e2 e3
  | A.Unop uop ->
    (*_ unary operator has double negation removal *)
    (match uop.op with
     | A.LogNot -> tr_exp_logNot env uop.operand
     | A.BitNot -> tr_exp_bitNot env uop.operand)

and tr_exp_ternary env e1 e2 e3 =
  let t = Temp.create () in
  let lt = Label.create () in
  let lf = Label.create () in
  let cmd1, exp1 = tr_exp_rev env e1 in
  let cmd2, exp2 = tr_exp_rev env e2 in
  let cmd3, exp3 = tr_exp_rev env e3 in
  let cond : T.cond = { cmp = T.Neq; p1 = exp1; p2 = T.Const (Int32.of_int_exn 0) } in
  let newcode =
    (*_ view in reverse order *)
    [ T.MovPureExp { dest = t; src = exp3 } :: cmd3
    ; T.Label lf :: T.MovPureExp { dest = t; src = exp2 } :: cmd2
    ; [ T.Label lt; T.If { cond; lt; lf } ]
    ; cmd1
    ]
    |> List.concat
  in
  newcode, T.Temp t

and tr_exp_logNot env e =
  let cmd, exp = tr_exp_rev env e in
  let normal =
    cmd, T.Binop { op = T.BitXor; lhs = exp; rhs = T.Const (Int32.of_int_exn 1) }
  in
  match Mark.data e with
  | A.Unop uop ->
    (match uop.op with
     | A.LogNot -> tr_exp_rev env uop.operand
     | _ -> normal)
  | _ -> normal

and tr_exp_bitNot env e =
  let cmd, exp = tr_exp_rev env e in
  let normal = cmd, T.Unop { op = T.BitNot; p = exp } in
  match Mark.data e with
  | A.Unop uop ->
    (match uop.op with
     | A.BitNot -> tr_exp_rev env uop.operand
     | _ -> normal)
  | _ -> normal
;;
(* 
let tr_exp (env : Temp.t S.t) (exp : A.mexp) : tr_exp_t =
  let stm_list, pexp = tr_exp_rev env exp in
  List.rev stm_list, pexp
;; *)

let rec tr_stm_rev (env : Temp.t S.t) (stm : A.program) =
  match Mark.data stm with
  | A.Declare decl ->
    let id = decl.var in
    let x = Temp.create () in
    let env' = S.set env ~key:id ~data:x in
    tr_stm_rev env' decl.body
  | A.Assign assn ->
    let id, e = assn.var, assn.exp in
    let x = S.find_exn env id in
    let cmd, exp = tr_exp_rev env e in
    T.MovPureExp { dest = x; src = exp } :: cmd
  | A.Return e ->
    let cmd, exp = tr_exp_rev env e in
    T.Return exp :: cmd
  | A.Nop -> []
  | A.Seq (s1, s2) -> tr_stm_rev env s2 @ tr_stm_rev env s1 (*_ reverse order! *)
  | A.NakedExpr e ->
    (*_ Bug source: should we assign fresh variable or should we ignore *)
    let t = Temp.create () in
    (*_ don't need to update env as no associated symbol *)
    let cmd, exp = tr_exp_rev env e in
    T.MovPureExp { dest = t; src = exp } :: cmd
  | A.If ifs -> tr_stm_if env ifs.cond ifs.lb ifs.rb
  | A.While loop -> tr_stm_while env loop.cond loop.body

and tr_stm_if env (cond : A.mexp) (s1 : A.program) (s2 : A.program) =
  let res1 = tr_stm_rev env s1 in
  let res2 = tr_stm_rev env s2 in
  let ccmd, cexp = tr_exp_rev env cond in
  let l1 = Label.create () in
  let l2 = Label.create () in
  let l3 = Label.create () in
  let tcond : T.cond = { cmp = T.Neq; p1 = cexp; p2 = T.Const (Int32.of_int_exn 0) } in
  let normal =
    (*_ view in reverse order! *)
    [ [ T.Label l3; T.Goto l3 ]
    ; res2
    ; [ T.Label l2; T.Goto l3 ]
    ; res1
    ; [ T.Label l1; T.If { cond = tcond; lt = l1; lf = l2 } ]
    ; ccmd
    ]
    |> List.concat
  in
  match Mark.data cond with
  | A.True -> tr_stm_rev env s1
  | A.False -> tr_stm_rev env s2
  | A.Unop uop ->
    (match uop.op with
     | A.LogNot -> tr_stm_if env uop.operand s2 s1
     | _ -> normal)
  | A.CmpBinop cop ->
    let op' = aste_binop_to_tree_cbop cop.op in
    let cmd1, exp1 = tr_exp_rev env cop.lhs in
    let cmd2, exp2 = tr_exp_rev env cop.rhs in
    let tcond : T.cond = { cmp = op'; p1 = exp1; p2 = exp2 } in
    (*_ view in reverse order *)
    [ T.Label l3 :: T.Goto l3 :: res2
    ; T.Label l2 :: T.Goto l3 :: res1
    ; T.Label l1 :: T.If { cond = tcond; lt = l1; lf = l2 } :: cmd2
    ; cmd1
    ]
    |> List.concat
  | _ -> normal

and tr_stm_while env (cond : A.mexp) (body : A.program) =
  let l1 = Label.create () in
  let l2 = Label.create () in
  let l3 = Label.create () in
  let rec cp e lt lf =
    let ccmd, cexp = tr_exp_rev env e in
    let tcond : T.cond = { cmp = T.Neq; p1 = cexp; p2 = T.Const (Int32.of_int_exn 0) } in
    let normal = T.If { cond = tcond; lt = l2; lf = l3 } :: ccmd in
    match Mark.data e with
    | A.True -> [ T.Goto lt ]
    | A.False -> [ T.Goto lf ]
    | A.Unop uop ->
      (match uop.op with
       | A.LogNot -> cp uop.operand lf lt
       | _ -> normal)
    | A.CmpBinop cop ->
      let cmp = aste_binop_to_tree_cbop cop.op in
      let cmd1, p1 = tr_exp_rev env cop.lhs in
      let cmd2, p2 = tr_exp_rev env cop.rhs in
      let tcond : T.cond = { cmp; p1; p2 } in
      T.If { cond = tcond; lt = l2; lf = l3 } :: (cmd2 @ cmd1)
    | _ -> normal
  in
  [ [ T.Label l3 ]
  ; T.Goto l1 :: tr_stm_rev env body
  ; T.Label l2 :: cp cond l2 l3
  ; [ T.Label l1 ]
  ]
  |> List.concat
;;

let tr_stm (env : Temp.t S.t) (stm : A.program) = List.rev (tr_stm_rev env stm)
let translate (prog : A.program) = tr_stm S.empty prog
