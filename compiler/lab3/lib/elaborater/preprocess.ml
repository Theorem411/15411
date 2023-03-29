open Core
module AstElab = Aste_l4
module Asts = Asts

let lookup (ctx : Symbol.Set.t) (f : Symbol.t) =
  Symbol.Set.exists ctx ~f:(fun s -> Symbol.equal f s)
;;

let rec rename_mexpr (ctx : Symbol.Set.t) (expsz : Asts.mexp) : Asts.mexp =
  let f = rename_mexpr ctx in
  let exp, sz = expsz in
  let e =
    match exp with
    | True | False | Var _ | Const _ | Alloc _ -> exp
    | Asts.Ternary { cond; lb; rb } ->
      Asts.Ternary { cond = f cond; lb = f lb; rb = f rb }
    | Asts.PureBinop { op; lhs; rhs } -> Asts.PureBinop { op; lhs = f lhs; rhs = f rhs }
    | Asts.EfktBinop { op; lhs; rhs } -> Asts.EfktBinop { op; lhs = f lhs; rhs = f rhs }
    | Asts.CmpBinop { op; lhs; rhs; size } ->
      Asts.CmpBinop { op; lhs = f lhs; rhs = f rhs; size }
    | Asts.Unop { op; operand } -> Asts.Unop { op; operand = f operand }
    | Asts.Call { name; args } ->
      if lookup ctx name
      then Asts.Call { name; args = List.map ~f args }
      else (
        let newname = Symbol.symbol ("_c0_" ^ Symbol.name name) in
        Asts.Call { name = newname; args = List.map ~f args })
    | Asts.Alloc_array { type_size; len } -> Asts.Alloc_array { type_size; len = f len }
    | ArrayAccess a -> ArrayAccess (rename_arraddr ctx a)
    | ArrAddr a -> ArrAddr (rename_arraddr ctx a)
    | StructAccess p -> StructAccess (rename_ptraddr ctx p)
    | Deref p -> Deref (rename_ptraddr ctx p)
    | PtrAddr p -> PtrAddr (rename_ptraddr ctx p)
  in
  e, sz

and rename_ptraddr ctx = function
  | Asts.Null -> Asts.Null
  | Ptr { start; off } -> Ptr { start = rename_mexpr ctx start; off }

and rename_arraddr ctx = function
  | { head; idx; size; extra } ->
    { head = rename_mexpr ctx head; idx = rename_mexpr ctx idx; size; extra }

and rename_stm (ctx : Symbol.Set.t) (stm : Asts.stm) : Asts.stm =
  let f = rename_mexpr ctx in
  let s = rename_stm ctx in
  match stm with
  | Asts.Declare { var; assign; body } ->
    Asts.Declare { var; assign = Option.map ~f assign; body = s body }
  | Asts.Assign { var; exp } -> Asts.Assign { var; exp = f exp }
  | Asts.If { cond; lb; rb } -> Asts.If { cond = f cond; lb = s lb; rb = s rb }
  | Asts.While { cond; body } -> Asts.While { cond = f cond; body = s body }
  | Asts.Return r -> Asts.Return (Option.map ~f r)
  | Asts.Seq (s1, s2) -> Asts.Seq (s s1, s s2)
  | Asts.NakedExpr e -> Asts.NakedExpr (f e)
  | Asts.NakedCall { name; args } ->
    if lookup ctx name
    then Asts.NakedCall { name; args = List.map ~f args }
    else (
      let newname = Symbol.symbol ("_c0_" ^ Symbol.name name) in
      Asts.NakedCall { name = newname; args = List.map ~f args })
  | AssignMem { dest; op; exp } -> AssignMem { dest; op; exp = f exp }
  | _ -> stm
;;

let rename_glob (ctx : Symbol.Set.t) (glob : Asts.glob) : Asts.glob =
  match glob with
  | { f; args; fdef } ->
    if lookup ctx f (* if lookup succes then do not rename *)
    then glob
    else (
      (* rename the function declaration*)
      let newname = Symbol.symbol ("_c0_" ^ Symbol.name glob.f) in
      { f = newname; args; fdef = rename_stm ctx fdef })
;;

let rename (header : AstElab.program) (source : Asts.program) : Asts.program =
  let get_ext_names (glob : AstElab.mglob) =
    match Mark.data glob with
    | AstElab.Fundef _ -> failwith "Found FunDef at in header"
    | AstElab.Fundecl { f; _ } -> Some f
    | _ -> None
  in
  let externalFuns : Symbol.Set.t =
    Symbol.Set.of_list (List.filter_map ~f:get_ext_names header)
  in
  if lookup externalFuns (Symbol.symbol "main")
  then failwith "header has main"
  else List.map ~f:(rename_glob externalFuns) source
;;
