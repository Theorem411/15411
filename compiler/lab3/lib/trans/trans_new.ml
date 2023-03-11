open Core
module T = Tree_new
module Typ = Ctype
module A = Aste
module S = Symbol.Map

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

type tr_exp_t = T.stm list * T.pexp

let rec tr_exp_rev (genv : Symbol.Set.t) (env : Temp.t S.t) (exp : A.mexp) : tr_exp_t =
  match Mark.data exp with
  | A.Var id -> [], T.Temp (S.find_exn env id)
  | A.Const n -> [], T.Const n
  | A.True -> [], T.Const (Int32.of_int_exn 1)
  | A.False -> [], T.Const (Int32.of_int_exn 0)
  | A.PureBinop bop ->
    let op' = aste_binop_to_tree_pbop bop.op in
    let cmd1, exp1 = tr_exp_rev genv env bop.lhs in
    let cmd2, exp2 = tr_exp_rev genv env bop.rhs in
    cmd2 @ cmd1, T.Binop { op = op'; lhs = exp1; rhs = exp2 }
  | A.EfktBinop eop ->
    let op' = aste_binop_to_tree_ebop eop.op in
    let cmd1, exp1 = tr_exp_rev genv env eop.lhs in
    let cmd2, exp2 = tr_exp_rev genv env eop.rhs in
    let t = Temp.create () in
    ( T.MovEfktExp { dest = t; ebop = op'; lhs = exp1; rhs = exp2 } :: (cmd2 @ cmd1)
    , T.Temp t )
  | A.CmpBinop cop ->
    let op' = aste_binop_to_tree_cbop cop.op in
    let cmd1, exp1 = tr_exp_rev genv env cop.lhs in
    let cmd2, exp2 = tr_exp_rev genv env cop.rhs in
    cmd2 @ cmd1, T.Cmpop { op = op'; lhs = exp1; rhs = exp2 }
  | A.Ternary tern ->
    let e1, e2, e3 = tern.cond, tern.lb, tern.rb in
    tr_exp_ternary genv env e1 e2 e3
  | A.Unop uop ->
    (*_ unary operator has double negation removal *)
    (match uop.op with
     | A.LogNot -> tr_exp_logNot genv env uop.operand
     | A.BitNot -> tr_exp_bitNot genv env uop.operand)
  | A.Call { name; args } ->
    let t = Temp.create () in
    (* let () = print_string (Symbol.pp_ss genv ^ "\n") in *)
    (* let () = print_string (Symbol.name name ^ "\n") in *)
    let fname = Symbol.Set.find_exn genv ~f:(fun s -> Symbol.equal s name) in
    let cmdllist, explist =
      List.fold
        args
        ~f:(fun (cll, el) arg ->
          let cmd, e = tr_exp_rev genv env arg in
          cmd :: cll, e :: el)
        ~init:([], [])
    in
    let cmdlist = List.concat cmdllist in
    T.MovFuncApp { dest = Some t; fname; args =  List.rev explist } :: cmdlist, T.Temp t

and tr_exp_ternary genv env e1 e2 e3 =
  let t = Temp.create () in
  let l1 = Label.create () in
  let l2 = Label.create () in
  let l3 = Label.create () in
  let cmd1, exp1 = tr_exp_rev genv env e1 in
  let cmd2, exp2 = tr_exp_rev genv env e2 in
  let cmd3, exp3 = tr_exp_rev genv env e3 in
  let cond : T.cond = { cmp = T.Neq; p1 = exp1; p2 = T.Const (Int32.of_int_exn 0) } in
  let newcode =
    (*_ view in reverse order *)
    [ [ T.Label l3 ]
    ; T.Goto l3 :: T.MovPureExp { dest = t; src = exp3 } :: cmd3
    ; [ T.Label l2 ]
    ; T.Goto l3 :: T.MovPureExp { dest = t; src = exp2 } :: cmd2
    ; [ T.Label l1 ]
    ; [ T.If { cond; lt = l1; lf = l2 } ]
    ; cmd1
    ]
    |> List.concat
  in
  newcode, T.Temp t

and tr_exp_logNot genv env e =
  match Mark.data e with
  | A.Unop uop ->
    (match uop.op with
     | A.LogNot -> tr_exp_rev genv env uop.operand
     | _ ->
       let cmd, exp = tr_exp_rev genv env e in
       cmd, T.Binop { op = T.BitXor; lhs = exp; rhs = T.Const (Int32.of_int_exn 1) })
  | _ ->
    let cmd, exp = tr_exp_rev genv env e in
    cmd, T.Binop { op = T.BitXor; lhs = exp; rhs = T.Const (Int32.of_int_exn 1) }

and tr_exp_bitNot genv env e =
  let normal_case env e =
    let cmd, exp = tr_exp_rev genv env e in
    cmd, T.Unop { op = T.BitNot; p = exp }
  in
  match Mark.data e with
  | A.Unop uop ->
    (match uop.op with
     | A.BitNot -> tr_exp_rev genv env uop.operand
     | _ -> normal_case env e)
  | _ -> normal_case env e
;;

let rec tr_stm_rev (genv : Symbol.Set.t) (env : Temp.t S.t) (stm : A.mstm) =
  match Mark.data stm with
  | A.Declare { var; assign; body; _ } ->
    let t = Temp.create () in
    let env' = S.set env ~key:var ~data:t in
    (match assign with
     | None -> tr_stm_rev genv env' body
     | Some exp ->
       let cmd, texp = tr_exp_rev genv env' exp in
       tr_stm_rev genv env' body @ T.MovPureExp { dest = t; src = texp } :: cmd)
  | A.Assign { var; exp } ->
    let x = S.find_exn env var in
    let cmd, texp = tr_exp_rev genv env exp in
    T.MovPureExp { dest = x; src = texp } :: cmd
  | A.Return eopt ->
    (match eopt with
     | Some e ->
       let cmd, exp = tr_exp_rev genv env e in
       (* let () = printf "show me commands %s" (List.map ~f:T.Print.pp_stm cmd|> String.concat) in
       let () = printf "show me return %s" (T.Print.pp_pexp exp) in *)
       T.Return (Some exp) :: cmd
     | None -> [ T.Return None ])
  | A.Nop -> []
  | A.Seq (s1, s2) ->
    tr_stm_rev genv env s2 @ tr_stm_rev genv env s1 (*_ reverse order! *)
  | A.NakedExpr e ->
    (*_ Bug source: should we assign fresh variable or should we ignore *)
    let t = Temp.create () in
    (*_ don't need to update env as no associated symbol *)
    let cmd, exp = tr_exp_rev genv env e in
    T.MovPureExp { dest = t; src = exp } :: cmd
  | A.If ifs -> tr_stm_if genv env ifs.cond ifs.lb ifs.rb
  | A.While loop -> tr_stm_while genv env loop.cond loop.body
  | A.AssertFail -> [ T.AssertFail ]
  | A.NakedCall { name; args } ->
    let fname = Symbol.Set.find_exn genv ~f:(fun s -> Symbol.equal s name) in
    let cmdllist, explist =
      List.fold
        args
        ~f:(fun (cll, el) arg ->
          let cmd, e = tr_exp_rev genv env arg in
          cmd :: cll, e :: el)
        ~init:([], [])
    in
    let cmdlist = List.concat cmdllist in
    T.MovFuncApp { dest = None; fname; args = List.rev explist } :: cmdlist

and tr_stm_if genv env (cond : A.mexp) (s1 : A.mstm) (s2 : A.mstm) =
  let normal_case (env : Temp.t S.t) cond s1 s2 =
    let res1 = tr_stm_rev genv env s1 in
    let res2 = tr_stm_rev genv env s2 in
    let ccmd, cexp = tr_exp_rev genv env cond in
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    let tcond : T.cond = { cmp = T.Neq; p1 = cexp; p2 = T.Const (Int32.of_int_exn 0) } in
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
  | A.True -> tr_stm_rev genv env s1
  | A.False -> tr_stm_rev genv env s2
  | A.Unop uop ->
    (match uop.op with
     | A.LogNot -> tr_stm_if genv env uop.operand s2 s1
     | _ -> normal_case env cond s1 s2)
  | A.CmpBinop cop ->
    let op' = aste_binop_to_tree_cbop cop.op in
    let cmd1, exp1 = tr_exp_rev genv env cop.lhs in
    let cmd2, exp2 = tr_exp_rev genv env cop.rhs in
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    let tcond : T.cond = { cmp = op'; p1 = exp1; p2 = exp2 } in
    (*_ view in reverse order *)
    [ T.Label l3 :: T.Goto l3 :: tr_stm_rev genv env s2
    ; T.Label l2 :: T.Goto l3 :: tr_stm_rev genv env s1
    ; T.Label l1 :: T.If { cond = tcond; lt = l1; lf = l2 } :: cmd2
    ; cmd1
    ]
    |> List.concat
  | _ -> normal_case env cond s1 s2

and tr_stm_while genv env (cond : A.mexp) (body : A.mstm) =
  let l1 = Label.create () in
  let l2 = Label.create () in
  let l3 = Label.create () in
  let rec cp e lt lf =
    match Mark.data e with
    | A.True -> [ T.Goto lt ]
    | A.False -> [ T.Goto lf ]
    | A.Unop { op = A.LogNot; operand } -> cp operand lf lt
    | A.Unop { op = A.BitNot; _ } -> cp_normal_case e lt lf
    | A.CmpBinop cop ->
      let cmp = aste_binop_to_tree_cbop cop.op in
      let cmd1, p1 = tr_exp_rev genv env cop.lhs in
      let cmd2, p2 = tr_exp_rev genv env cop.rhs in
      let tcond : T.cond = { cmp; p1; p2 } in
      T.If { cond = tcond; lt; lf } :: (cmd2 @ cmd1)
    | _ -> cp_normal_case e lt lf
  and cp_normal_case e lt lf =
    let ccmd, cexp = tr_exp_rev genv env e in
    let tcond : T.cond = { cmp = T.Neq; p1 = cexp; p2 = T.Const (Int32.of_int_exn 0) } in
    T.If { cond = tcond; lt; lf } :: ccmd
  in
  let loop_guard_code = cp cond l2 l3 in
  let loop_body_code = tr_stm_rev genv env body in
  [ [ T.Label l3 ]
  ; T.Goto l1 :: loop_body_code
  ; T.Label l2 :: loop_guard_code
  ; [ T.Label l1 ]
  ]
  |> List.concat
;;

let tr_stm (genv : Symbol.Set.t) (env : Temp.t S.t) (stm : A.mstm) =
  List.rev (tr_stm_rev genv env stm)
;;

let args_tag (sl : Symbol.t list) =
  let s2t = List.map sl ~f:(fun s -> let t = Temp.create () in (s, t)) in
  let sset = S.of_alist_exn s2t in
  let _, tlst = List.unzip s2t in
  (* List.fold ~init:([], S.empty) ~f:(fun (args, acc) s ->
    let t = Temp.create () in
    t :: args, S.set acc ~key:s ~data:t) sl *)
    tlst, sset
;;

let break_into_blocks (stms : T.stm list) : T.block list =
  let is_jmp s =
    match s with
    | T.If _ -> true
    | T.Return _ -> true
    | T.Goto _ -> true
    | _ -> false
  in
  let is_label s =
    match s with
    | T.Label _ -> true
    | _ -> false
  in
  let break l break_l break_r =
    let gap_r = List.group ~break:break_r l in
    let map_f ss = List.group ~break:break_l ss in
    let gap_l = List.map ~f:map_f gap_r |> List.concat in
    List.filter ~f:(fun ss -> if List.length ss > 0 then true else false) gap_l
  in
  let break_by_jmp s _ = is_jmp s in
  let break_by_lab _ s = is_label s in
  let pass1 = break stms break_by_lab break_by_jmp in
  let prelab sl =
    let hd = List.hd_exn sl in
    if is_label hd
    then sl
    else (
      let l = Label.create () in
      T.Label l :: sl)
  in
  let pass1_prelab = List.map pass1 ~f:prelab in
  let endjmp sl =
    let tl = List.last_exn sl in
    let sl_opt = List.map sl ~f:(fun a -> Some a) in
    if is_jmp tl then sl_opt else sl_opt @ [ None ]
  in
  let pass1_endjmp = List.map pass1_prelab ~f:endjmp |> List.concat in
  let rec borrow stm_opt_list =
    match stm_opt_list with
    | [] -> []
    | Some s :: rest -> s :: borrow rest
    | None :: rest ->
      (match rest with
       | Some (T.Label l) :: _ -> T.Goto l :: borrow rest
       | _ -> failwith "impossible in break_into_blocks!")
  in
  let pass1_borrow = borrow pass1_endjmp in
  let pass2 = break pass1_borrow break_by_lab break_by_jmp in
  let map_sblock_block (sblock : T.stm list) : T.block =
    let get_label_exn = function
      | T.Label l -> l
      | _ -> failwith "impossible in break_into_blocks"
    in
    let hd = List.hd_exn sblock in
    let label = get_label_exn hd in
    let tl = List.last_exn sblock in
    { label; block = sblock; jump = tl }
  in
  List.map pass2 ~f:map_sblock_block
;;

let tr_glob (glob : A.mglob) (global_env : Symbol.Set.t) =
  match Mark.data glob with
  | A.Typedef _ -> None, global_env
  | A.Fundecl { f; _ } ->
    let global_env' = Symbol.Set.add global_env f in
    None, global_env'
  | A.Fundef { f; args; fdef; _ } ->
    (* let () = printf "f defined %s: %s\n" (Symbol.name f) (A.Print.pp_mstm fdef) in *)
    let args_lst, args_env = args_tag args in
    let global_env' = Symbol.Set.add global_env f in
    let fdef = tr_stm global_env' args_env fdef in
    (*_ |> break_into_blocks *)
    Some ({ fname = f; args = args_lst; fdef } : T.fspace), global_env'
;;

let translate (prog : A.program) : T.program =
  let foldf (acc, genv) g =
    let res, genv' = tr_glob g genv in
    match res with
    | None -> acc, genv'
    | Some fspace -> fspace :: acc, genv'
  in
  let tprog, _ = List.fold prog ~init:([], Symbol.Set.empty) ~f:foldf in
  tprog |> List.rev
;;

let translate_block (tprog : T.program) : T.program_block =
  let map_f ({ fname; args; fdef } : T.fspace) : T.fspace_block =
    { fname; args; fdef = break_into_blocks fdef }
  in
  List.map tprog ~f:map_f
;;
