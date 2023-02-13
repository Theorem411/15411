open Core

(*_ 
1. Remove invalid assignments (x + 1) = 2;  
-> 2. Do elaboration
*)

let to_pure = function
  | Ast.Plus -> Aste.Plus
  | Ast.Minus -> Aste.Minus
  | Ast.Times -> Aste.Times
  | Ast.B_and -> Aste.And
  | Ast.B_or -> Aste.Or
  | Ast.B_xor -> Aste.Xor
  | _ -> failwith "Not a pure binop"
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

let check_rec_declaration (_ : Symbol.t) (_ : Ast.mexp) : unit =
  (* failwith "not implemented yet!" *)
  ()
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
  (* TODO fix *)
  | Some p -> Mark.naked (Ast.Block [ p; body ])
;;

(* Turn Ast.For to Ast.While and then use other elab functions:) *)
let for_to_while m_s : Ast.stm Mark.t =
  Mark.map
    ~f:(fun s ->
      match s with
      | Ast.For { init; cond; body; post } ->
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
  | Const n -> copy_mark m_e (Aste.IntConst n)
  | True -> copy_mark m_e Aste.True
  | False -> copy_mark m_e Aste.False
  | Unop { op; operand } ->
    (match op with
    (* turning -x to 0 - x *)
    | Ast.Negative ->
      copy_mark
        m_e
        (Aste.PureBinop
           { op = Aste.Plus
           ; lhs = copy_mark m_e (Aste.IntConst Int32.zero)
           ; rhs = elab_mexp operand
           })
    | Ast.L_not ->
      copy_mark m_e (Aste.Unop { op = Aste.L_not; operand = elab_mexp operand })
    | Ast.B_not ->
      copy_mark m_e (Aste.Unop { op = Aste.B_not; operand = elab_mexp operand }))
  | Ternary { cond; first; second } ->
    copy_mark
      m_e
      (Aste.Ternary { cond = elab_mexp cond; lb = elab_mexp first; rb = elab_mexp second })
  (* turning logical ops to ternary*)
  | Binop { op = Ast.L_and | Ast.L_or; lhs; rhs } ->
    elab_mexp
      (copy_mark
         m_e
         (Ast.Ternary { cond = lhs; first = rhs; second = copy_mark m_e Ast.False }))
  (* pure *)
  | Binop
      { op = (Ast.Plus | Ast.Minus | Ast.Times | Ast.B_and | Ast.B_or | Ast.B_xor) as op
      ; lhs
      ; rhs
      } ->
    copy_mark
      m_e
      (Aste.PureBinop { op = to_pure op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
  (* efkt *)
  | Binop { op; lhs; rhs } ->
    copy_mark
      m_e
      (Aste.EfktBinop { op = to_efkt op; lhs = elab_mexp lhs; rhs = elab_mexp rhs })
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
                   ; rhs = copy_mark m_l (Aste.IntConst Int32.one)
                   })
          }
      | _ -> failwith "post op is not + or -")
    | _ -> failwith "LHS is not a var")
  | _ -> failwith "elab_postop recieved not a postop"
;;

let elab_return m_s =
  match Mark.data m_s with
  | Ast.Return m_r -> Aste.Return (elab_mexp m_r)
  | _ -> failwith "elab_return recieved not a return"
;;

let rec elab_if m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.If { cond; thenstm; elsestm } ->
    let c = elab_mexp cond in
    let t : Aste.stmt Mark.t = elab thenstm in
    let f : Aste.stmt Mark.t =
      match elsestm with
      | None -> Mark.naked Aste.Nop
      | Some elsepart -> elab elsepart
    in
    Aste.If { cond = c; lb = t; rb = f }
  | _ -> failwith "elab_if recieved not an if"

and elab_while m_s =
  let s = Mark.data m_s in
  match s with
  | Ast.While { cond; body } ->
    let c = elab_mexp cond in
    let b : Aste.stmt Mark.t = elab body in
    Aste.While { cond = c; body = b }
  | _ -> failwith "elab_while recieved not a while"

and elab_for m_s = elab (for_to_while m_s)

and elab m_s : Aste.stmt Mark.t =
  match Mark.data m_s with
  (* special cases *)
  | Ast.Block mstms -> elaborate_stmts mstms
  | Ast.Declare _ -> failwith "declare should be handled in elaborate"
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

and elaborate_stmts (p : Ast.mstm list) : Aste.program =
  let () = List.iter ~f:vldt_assign p in
  match p with
  | [] -> Mark.naked Aste.Nop
  | mx :: mstsms ->
    copy_mark
      mx
      (match Mark.data mx with
      | Ast.Declare (Ast.New_var (var, typ)) ->
        Aste.Declare { var; typ; body = elaborate_stmts mstsms }
      | Ast.Declare (Ast.Init (var, typ, m_e)) ->
        let () = check_rec_declaration var m_e in
        (*_ checking if int x = x + 1;*)
        Aste.Declare
          { var
          ; typ
          ; body =
              copy_mark
                mx
                (Aste.Seq
                   ( copy_mark mx (Aste.Assign { var; exp = elab_mexp m_e })
                   , elaborate_stmts mstsms ))
          }
      | _ -> Aste.Seq (elab mx, elaborate_stmts mstsms))
;;

let elaborate (p : Ast.mstm list) : Aste.program = elaborate_stmts p
