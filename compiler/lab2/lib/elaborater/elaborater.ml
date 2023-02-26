open Core
module Typ = Ast.T
(* module SymbolMap = Hashtbl.Make (Symbol) *)

let to_pure = function
  | Ast.Plus -> Aste.Plus
  | Ast.Minus -> Aste.Minus
  | Ast.Times -> Aste.Times
  | Ast.B_and -> Aste.BitAnd
  | Ast.B_or -> Aste.BitOr
  | Ast.B_xor -> Aste.BitXor
  | _ -> failwith "Not a pure binop"
;;

let to_fsig (params : Ast.param list) (ret_type : Typ.tau option) : Typ.fsig =
  let param_tuple = List.map ~f:(fun (Ast.Param { t; name }) -> name, t) params in
  let m : Typ.tau Symbol.Map.t = Symbol.Map.of_alist_exn param_tuple in
  m, ret_type
;;

let to_cmp = function
  | Ast.Greater -> Aste.Greater
  | Ast.Greater_eq -> Aste.Geq
  | Ast.Less -> Aste.Less
  | Ast.Less_eq -> Aste.Leq
  | Ast.Equals -> Aste.Eq
  | Ast.Not_equals -> Aste.Neq
  | _ -> failwith "Not a cmp binp"
;;

let to_efkt = function
  | Ast.Divided_by -> Aste.Divided_by
  | Ast.Modulo -> Aste.Modulo
  | Ast.ShiftL -> Aste.ShiftL
  | Ast.ShiftR -> Aste.ShiftR
  | _ -> failwith "Not an effect binop"
;;

let copy_mark (a : 'a Mark.t) (b : 'b) : 'b Mark.t = Mark.map ~f:(fun _ -> b) a

let give_out (l : 'a Mark.t) =
  match Mark.src_span l with
  | None -> ""
  | Some src -> Mark.show src
;;

let rec check_rec_dcl (v : Symbol.t) (e : Ast.exp) : unit =
  match e with
  | Ast.Var v2 ->
    if Symbol.equal v v2
    then failwith ("recursive defintion was found for " ^ Symbol.name v)
  | Ast.Binop { lhs; rhs; _ } ->
    List.iter ~f:(check_rec_dcl v) (List.map ~f:Mark.data [ lhs; rhs ])
  | Ast.Ternary { cond; first; second } ->
    List.iter ~f:(check_rec_dcl v) (List.map ~f:Mark.data [ cond; first; second ])
  | Ast.Unop { operand; _ } -> check_rec_dcl v (Mark.data operand)
  | _ -> ()
;;

let vldt_assign m =
  match Mark.data m with
  | Ast.Assign { left = l; _ } ->
    (match Mark.data l with
    | Ast.Var _ -> ()
    | _ -> raise (Failure ("Invalid asign" ^ give_out l)))
  | _ -> ()
;;

let update_body (body : Ast.mstm) (post : Ast.mstm option) =
  match post with
  | None -> body
  (* TODO fix marks *)
  | Some p ->
    (match Mark.data p with
    | Ast.Declare _ -> failwith "post of for loop can not be a decl"
    | _ -> copy_mark p (Ast.Block [ body; p ]))
;;

(* Turn Ast.For to Ast.While and then use other elab functions:) *)
let for_to_while m_s : Ast.stm Mark.t =
  Mark.map
    ~f:(fun s ->
      match s with
      | Ast.For { init; cond; body; post } ->
        (* also checks that third statement is not declaration *)
        let while_loop = Ast.While { cond; body = update_body body post } in
        (match init with
        | None -> while_loop
        | Some i' -> Ast.Block [ i'; copy_mark m_s while_loop ])
      | _ -> failwith "transforming not for .... :(")
    m_s
;;

let rec elab_mexp (m_e : Ast.mexp) : Aste.mexp =
  let e = Mark.data m_e in
  match e with
  | Var s -> copy_mark m_e (Aste.Var s)
  | Const n -> copy_mark m_e (Aste.Const n)
  | True -> copy_mark m_e Aste.True
  | False -> copy_mark m_e Aste.False
  | Unop { op; operand } ->
    (match op with
    (* turning -x to 0 - x *)
    | Ast.Negative ->
      copy_mark
        m_e
        (Aste.PureBinop
           { op = Aste.Minus
           ; lhs = copy_mark m_e (Aste.Const Int32.zero)
           ; rhs = elab_mexp operand
           })
    | Ast.L_not ->
      copy_mark m_e (Aste.Unop { op = Aste.LogNot; operand = elab_mexp operand })
    | Ast.B_not ->
      copy_mark m_e (Aste.Unop { op = Aste.BitNot; operand = elab_mexp operand }))
  | Ternary { cond; first; second } ->
    copy_mark
      m_e
      (Aste.Ternary { cond = elab_mexp cond; lb = elab_mexp first; rb = elab_mexp second })
  (* turning logical ops to ternary*)
  | Binop { op = Ast.L_and; lhs; rhs } ->
    elab_mexp
      (copy_mark
         m_e
         (Ast.Ternary { cond = lhs; first = rhs; second = copy_mark m_e Ast.False }))
  | Binop { op = Ast.L_or; lhs; rhs } ->
    elab_mexp
      (copy_mark
         m_e
         (Ast.Ternary { cond = lhs; first = copy_mark m_e Ast.True; second = rhs }))
  (* pure *)
  | Binop
      { op = (Ast.Plus | Ast.Minus | Ast.Times | Ast.B_and | Ast.B_or | Ast.B_xor) as op
      ; lhs
      ; rhs
      } ->
    copy_mark
      m_e
      (Aste.PureBinop { op = to_pure op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
  (* cmp *)
  | Binop
      { op =
          ( Ast.Equals
          | Ast.Greater
          | Ast.Greater_eq
          | Ast.Less
          | Ast.Less_eq
          | Ast.Not_equals ) as op
      ; lhs
      ; rhs
      } ->
    copy_mark
      m_e
      (Aste.CmpBinop { op = to_cmp op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
  (* efkt *)
  | Binop { op; lhs; rhs } ->
    copy_mark
      m_e
      (Aste.EfktBinop { op = to_efkt op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
  | Ast.Call { name; args } ->
    let argsnew = List.map ~f:elab_mexp args in
    copy_mark m_e (Aste.Call { name; args = argsnew })
;;

let elab_assign_with_op
    (m_l : Ast.mexp)
    (v : Symbol.t)
    (op : Ast.binop)
    (m_r : Ast.mexp)
    (m_s : Ast.stm Mark.t)
  =
  let expr_new =
    match op with
    | Plus | Minus | Times | B_and | B_or | B_xor ->
      Aste.PureBinop { op = to_pure op; lhs = elab_mexp m_l; rhs = elab_mexp m_r }
    | Divided_by | Modulo | ShiftL | ShiftR ->
      Aste.EfktBinop { op = to_efkt op; lhs = elab_mexp m_l; rhs = elab_mexp m_r }
    | Greater | Greater_eq | Less_eq | Less | Equals | Not_equals ->
      Aste.CmpBinop { op = to_cmp op; lhs = elab_mexp m_l; rhs = elab_mexp m_r }
    | _ -> failwith "Not asign with op"
  in
  Aste.Assign { var = v; exp = copy_mark m_s expr_new }
;;

let elab_assign m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.Assign { left = m_l; right = m_r; asgnop } ->
    let l = Mark.data m_l in
    (match l with
    | Ast.Var v ->
      (match asgnop with
      | None -> Aste.Assign { var = v; exp = elab_mexp m_r }
      | Some op -> elab_assign_with_op m_l v op m_r m_s)
    | _ -> failwith "LHS is not a var")
  | _ -> failwith "elab_assign recieved not assign"
;;

let elab_postop m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.PostOp { left = m_l; op } ->
    let l = Mark.data m_l in
    (match l with
    | Ast.Var v ->
      (match op with
      | Ast.Plus | Ast.Minus ->
        Aste.Assign
          { var = v
          ; exp =
              copy_mark
                m_s
                (Aste.PureBinop
                   { op = to_pure op
                   ; lhs = elab_mexp m_l
                   ; rhs = copy_mark m_l (Aste.Const Int32.one)
                   })
          }
      | _ -> failwith "post op is not + or -")
    | _ -> failwith "LHS is not a var")
  | _ -> failwith "elab_postop recieved not a postop"
;;

let elab_return m_s =
  let helper (m_r : Ast.mexp option) =
    match m_r with
    | None -> None
    | Some r -> Some (elab_mexp r)
  in
  match Mark.data m_s with
  | Ast.Return m_r -> Aste.Return (helper m_r)
  | _ -> failwith "elab_return recieved not a return"
;;

let rec elab_if m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.If { cond; thenstm; elsestm } ->
    let c = elab_mexp cond in
    let t : Aste.stm Mark.t = elaborate_stmts [ thenstm ] in
    let f : Aste.stm Mark.t =
      match elsestm with
      | None -> Mark.naked Aste.Nop
      | Some elsepart -> elaborate_stmts [ elsepart ]
    in
    Aste.If { cond = c; lb = t; rb = f }
  | _ -> failwith "elab_if recieved not an if"

and elab_while m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.While { cond; body } ->
    let c = elab_mexp cond in
    let b : Aste.stm Mark.t = elaborate_stmts [ body ] in
    Aste.While { cond = c; body = b }
  | _ -> failwith "elab_while recieved not a while"

and elab_for m_s = elab (for_to_while m_s)

and elab m_s : Aste.stm Mark.t =
  match Mark.data m_s with
  (* special cases *)
  | Ast.Block mstms -> elaborate_stmts mstms
  | Ast.Declare _ -> failwith "declare should be handled in elaborate_stmts function"
  | Ast.For _ -> elab_for m_s
  (* Nice way to still have copy_mark m_s wrapper around cases who need them *)
  | _ ->
    copy_mark
      m_s
      (match Mark.data m_s with
      | Ast.Nop -> Aste.Nop
      | Ast.Assign _ -> elab_assign m_s
      | Ast.PostOp _ -> elab_postop m_s
      | Ast.Return _ -> elab_return m_s
      | Ast.Exp e -> Aste.NakedExpr (elab_mexp e)
      | Ast.If _ -> elab_if m_s
      | Ast.While _ -> elab_while m_s
      | _ -> failwith "should not reach here")

and elaborate_stmts (p : Ast.mstm list) : Aste.mstm =
  let () = List.iter ~f:vldt_assign p in
  match p with
  | [] -> Mark.naked Aste.Nop
  | mx :: mstsms ->
    copy_mark
      mx
      (match Mark.data mx with
      | Ast.Declare (Ast.New_var (var, typ)) ->
        Aste.Declare { var; typ; body = elaborate_stmts mstsms; assign = None }
      | Ast.Declare (Ast.Init (var, typ, m_e)) ->
        (* may I can remove this check? *)
        let () = check_rec_dcl var (Mark.data m_e) in
        (*_ checking if int x = x + 1;*)
        Aste.Declare
          { var; typ; assign = Some (elab_mexp m_e); body = elaborate_stmts mstsms }
      | _ -> Aste.Seq (elab mx, elaborate_stmts mstsms))


and elaborate' (s : Ast.mgdecl) : Aste.mglob =
  let res =
    match Mark.data s with
    | Ast.Typedef { old_name; new_name } -> Aste.Typedef (old_name, new_name)
    | Ast.FunDec { name; ret_type; params } -> Aste.Fundecl (name, to_fsig params ret_type)
    | Ast.FunDef { name; ret_type; params; body } ->
      Aste.Fundef (name, to_fsig params ret_type, elaborate_stmts [ body ])
  in
  copy_mark s res

and elaborate p : Aste.program = List.map ~f:elaborate' p
