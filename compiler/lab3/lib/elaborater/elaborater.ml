open Core
module Typ = Ast.T
module Ast = Ast
module AstElab = Aste_l4
(* module SymbolMap = Hashtbl.Make (Symbol) *)

let to_pure = function
  | Ast.Plus -> AstElab.Plus
  | Ast.Minus -> AstElab.Minus
  | Ast.Times -> AstElab.Times
  | Ast.B_and -> AstElab.BitAnd
  | Ast.B_or -> AstElab.BitOr
  | Ast.B_xor -> AstElab.BitXor
  | _ -> failwith "Not a pure binop"
;;

let to_fsig (params : Ast.param list) (ret_type : Typ.tau option) : Typ.fsig =
  List.map ~f:(fun (Ast.Param { t; _ }) -> t) params, ret_type
;;

let to_arg_list (params : Ast.param list) : Symbol.t list =
  List.map ~f:(fun (Ast.Param { name; _ }) -> name) params
;;

let to_cmp = function
  | Ast.Greater -> AstElab.Greater
  | Ast.Greater_eq -> AstElab.Geq
  | Ast.Less -> AstElab.Less
  | Ast.Less_eq -> AstElab.Leq
  | Ast.Equals -> AstElab.Eq
  | Ast.Not_equals -> AstElab.Neq
  | _ -> failwith "Not a cmp binp"
;;

let to_efkt = function
  | Ast.Divided_by -> AstElab.Divided_by
  | Ast.Modulo -> AstElab.Modulo
  | Ast.ShiftL -> AstElab.ShiftL
  | Ast.ShiftR -> AstElab.ShiftR
  | _ -> failwith "Not an effect binop"
;;

let to_binop : Ast.binop option -> AstElab.binop option = function
  | None -> None
  | Some b ->
    (match b with
    | Plus -> Some (AstElab.Pure AstElab.Plus)
    | Minus -> Some (AstElab.Pure AstElab.Minus)
    | Times -> Some (AstElab.Pure AstElab.Times)
    | Divided_by -> Some (AstElab.Efkt AstElab.Divided_by)
    | Modulo -> Some (AstElab.Efkt AstElab.Modulo)
    | B_and -> Some (AstElab.Pure AstElab.BitAnd)
    | B_xor -> Some (AstElab.Pure AstElab.BitXor)
    | B_or -> Some (AstElab.Pure AstElab.BitOr)
    | ShiftL -> Some (AstElab.Efkt AstElab.ShiftL)
    | ShiftR -> Some (AstElab.Efkt AstElab.ShiftR)
    | Less | Less_eq | Greater | Greater_eq | Equals | Not_equals | L_and | L_or ->
      failwith "Not expected to get compare in to_binop")
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

(*_ validates if m is the lvalue *)
let vldt_assign m =
  match Mark.data m with
  | Ast.Assign { left = l; _ } ->
    (match Mark.data l with
    | Ast.Var _ -> ()
    | Ast.StructDot _ -> ()
    | Ast.StructArr _ -> ()
    | Ast.ArrAccess _ -> ()
    | Ast.Deref _ -> ()
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

let rec elab_mexp (m_e : Ast.mexp) : AstElab.mexp =
  let e = Mark.data m_e in
  match e with
  | Var s -> copy_mark m_e (AstElab.Var s)
  | Const n -> copy_mark m_e (AstElab.Const n)
  | True -> copy_mark m_e AstElab.True
  | False -> copy_mark m_e AstElab.False
  | Unop { op; operand } ->
    (match op with
    (* turning -x to 0 - x *)
    | Ast.Negative ->
      copy_mark
        m_e
        (AstElab.PureBinop
           { op = AstElab.Minus
           ; lhs = copy_mark m_e (AstElab.Const Int32.zero)
           ; rhs = elab_mexp operand
           })
    | Ast.L_not ->
      copy_mark m_e (AstElab.Unop { op = AstElab.LogNot; operand = elab_mexp operand })
    | Ast.B_not ->
      copy_mark m_e (AstElab.Unop { op = AstElab.BitNot; operand = elab_mexp operand }))
  | Ternary { cond; first; second } ->
    copy_mark
      m_e
      (AstElab.Ternary
         { cond = elab_mexp cond; lb = elab_mexp first; rb = elab_mexp second })
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
      (AstElab.PureBinop { op = to_pure op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
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
      (AstElab.CmpBinop { op = to_cmp op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
  (* efkt *)
  | Binop { op; lhs; rhs } ->
    copy_mark
      m_e
      (AstElab.EfktBinop { op = to_efkt op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
  | Ast.Call { name; args } ->
    let argsnew = List.map ~f:elab_mexp args in
    copy_mark m_e (AstElab.Call { name; args = argsnew })
  | Ast.Null -> copy_mark m_e AstElab.Null
  | Deref exp -> copy_mark m_e (AstElab.Deref (elab_mexp exp))
  | ArrAccess { arr; idx } ->
    copy_mark m_e (AstElab.ArrAccess { arr = elab_mexp arr; idx = elab_mexp idx })
  | StructDot { str; field } ->
    copy_mark m_e (AstElab.StructDot { field; str = elab_mexp str })
  | StructArr { str; field } ->
    copy_mark m_e (AstElab.StructArr { field; str = elab_mexp str })
  | Alloc t -> copy_mark m_e (AstElab.Alloc t)
  | Alloc_array { typ; len } ->
    copy_mark m_e (AstElab.Alloc_array { typ; len = elab_mexp len })
;;

let elab_assign_with_op_to_var
    (m_l : Ast.mexp)
    (v : Symbol.t)
    (op : Ast.binop)
    (m_r : Ast.mexp)
    (m_s : Ast.stm Mark.t)
  =
  let expr_new =
    match op with
    | Plus | Minus | Times | B_and | B_or | B_xor ->
      AstElab.PureBinop { op = to_pure op; lhs = elab_mexp m_l; rhs = elab_mexp m_r }
    | Divided_by | Modulo | ShiftL | ShiftR ->
      AstElab.EfktBinop { op = to_efkt op; lhs = elab_mexp m_l; rhs = elab_mexp m_r }
    | Greater | Greater_eq | Less_eq | Less | Equals | Not_equals ->
      AstElab.CmpBinop { op = to_cmp op; lhs = elab_mexp m_l; rhs = elab_mexp m_r }
    | _ -> failwith "Not asign with op"
  in
  AstElab.AssignToSymbol { var = v; exp = copy_mark m_s expr_new }
;;

let elab_assign m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.Assign { left = m_l; right = m_r; asgnop } ->
    let l = Mark.data m_l in
    (match l with
    | Ast.Var v ->
      (match asgnop with
      | None -> AstElab.AssignToSymbol { var = v; exp = elab_mexp m_r }
      | Some op -> elab_assign_with_op_to_var m_l v op m_r m_s)
    | Ast.StructDot _ | Ast.StructArr _ | Ast.ArrAccess _ | Ast.Deref _ ->
      AstElab.Assign { dest = elab_mexp m_l; exp = elab_mexp m_r; op = to_binop asgnop }
    | _ -> failwith "LHS is not a lvalue")
  | _ -> failwith "elab_assign recieved not assign"
;;

let elab_postop m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.PostOp { left = m_l; op } ->
    (match op with
    | Ast.Plus ->
      elab_assign
        (copy_mark
           m_s
           (Ast.Assign
              { left = m_l
              ; asgnop = Some Ast.Plus
              ; right = copy_mark m_s (Ast.Const Int32.one)
              }))
    | Ast.Minus ->
      elab_assign
        (copy_mark
           m_s
           (Ast.Assign
              { left = m_l
              ; asgnop = Some Ast.Minus
              ; right = copy_mark m_s (Ast.Const Int32.one)
              }))
    | _ -> failwith "post op is not + or -")
  | _ -> failwith "elab_postop recieved not a postop"
;;

let elab_return m_s =
  let helper (m_r : Ast.mexp option) =
    match m_r with
    | None -> None
    | Some r -> Some (elab_mexp r)
  in
  match Mark.data m_s with
  | Ast.Return m_r -> AstElab.Return (helper m_r)
  | _ -> failwith "elab_return recieved not a return"
;;

let rec elab_if m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.If { cond; thenstm; elsestm } ->
    let c = elab_mexp cond in
    let t : AstElab.stm Mark.t = elaborate_stmts [ thenstm ] in
    let f : AstElab.stm Mark.t =
      match elsestm with
      | None -> Mark.naked AstElab.Nop
      | Some elsepart -> elaborate_stmts [ elsepart ]
    in
    AstElab.If { cond = c; lb = t; rb = f }
  | _ -> failwith "elab_if recieved not an if"

and elab_while m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.While { cond; body } ->
    let c = elab_mexp cond in
    let b : AstElab.stm Mark.t = elaborate_stmts [ body ] in
    AstElab.While { cond = c; body = b }
  | _ -> failwith "elab_while recieved not a while"

and elab_assert m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.Assert e ->
    AstElab.If
      { cond = elab_mexp e
      ; lb = copy_mark m_s AstElab.Nop
      ; rb = copy_mark m_s AstElab.AssertFail
      }
  | _ -> failwith "elab_assert recieved not an assert"

and elab_for m_s = elab (for_to_while m_s)

and elab_stm_exp e =
  let s = Mark.data e in
  match s with
  | Ast.Call { name; args } ->
    let argsnew = List.map ~f:elab_mexp args in
    AstElab.NakedCall { name; args = argsnew }
  | _ -> AstElab.NakedExpr (elab_mexp e)

and elab m_s : AstElab.stm Mark.t =
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
      | Ast.Nop -> AstElab.Nop
      | Ast.Assign _ -> elab_assign m_s
      | Ast.PostOp _ -> elab_postop m_s
      | Ast.Return _ -> elab_return m_s
      | Ast.Exp e -> elab_stm_exp e
      | Ast.If _ -> elab_if m_s
      | Ast.While _ -> elab_while m_s
      | Ast.Assert _ -> elab_assert m_s
      | _ -> failwith "should not reach here")

and elaborate_stmts (p : Ast.mstm list) : AstElab.mstm =
  let () = List.iter ~f:vldt_assign p in
  match p with
  | [] -> Mark.naked AstElab.Nop
  | mx :: mstsms ->
    copy_mark
      mx
      (match Mark.data mx with
      | Ast.Declare (Ast.New_var (var, typ)) ->
        AstElab.Declare { var; typ; body = elaborate_stmts mstsms; assign = None }
      | Ast.Declare (Ast.Init (var, typ, m_e)) ->
        (* may I can remove this check? *)
        let () = check_rec_dcl var (Mark.data m_e) in
        (*_ checking if int x = x + 1;*)
        AstElab.Declare
          { var; typ; assign = Some (elab_mexp m_e); body = elaborate_stmts mstsms }
      | _ -> AstElab.Seq (elab mx, elaborate_stmts mstsms))

and elaborate' (s : Ast.mgdecl) : AstElab.mglob =
  let res =
    match Mark.data s with
    | Ast.Typedef { old_name; new_name } ->
      AstElab.Typedef { told = old_name; tnew = new_name }
    | Ast.FunDec { name; ret_type; params } ->
      AstElab.Fundecl
        { fsig = to_fsig params ret_type; f = name; args = to_arg_list params }
    | Ast.FunDef { name; ret_type; params; body } ->
      AstElab.Fundef
        { fsig = to_fsig params ret_type
        ; f = name
        ; args = to_arg_list params
        ; fdef = elaborate_stmts [ body ]
        }
    | Ast.Sdecl s -> AstElab.Sdecl s
    | Ast.Sdef { sname; ssig } -> AstElab.Sdef { sname; ssig }
  in
  copy_mark s res

and elaborate p : AstElab.program = List.map ~f:elaborate' p

let add_main p : AstElab.program =
  [ Mark.naked
      (AstElab.Fundecl
         { fsig = to_fsig [] (Some (Typ.RealTyp Typ.Int))
         ; f = Symbol.symbol "main"
         ; args = []
         })
  ]
  @ p
;;