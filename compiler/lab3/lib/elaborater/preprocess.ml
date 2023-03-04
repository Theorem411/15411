open Core
module AstElab = Aste

let lookup (ctx : Symbol.Set.t) (f : Symbol.t) =
  Symbol.Set.exists ctx ~f:(fun s -> Symbol.equal f s)
;;

let copy_mark (a : 'a Mark.t) (b : 'b) : 'b Mark.t = Mark.map ~f:(fun _ -> b) a

let rec rename_mexpr (ctx : Symbol.Set.t) (mexp : AstElab.mexp) : AstElab.mexp =
  let f = rename_mexpr ctx in
  let exp = Mark.data mexp in
  match exp with
  | AstElab.Ternary { cond; lb; rb } ->
    AstElab.Ternary { cond = f cond; lb = f lb; rb = f rb } |> copy_mark mexp
  | AstElab.PureBinop { op; lhs; rhs } ->
    AstElab.PureBinop { op; lhs = f lhs; rhs = f rhs } |> copy_mark mexp
  | AstElab.EfktBinop { op; lhs; rhs } ->
    AstElab.EfktBinop { op; lhs = f lhs; rhs = f rhs } |> copy_mark mexp
  | AstElab.CmpBinop { op; lhs; rhs } ->
    AstElab.CmpBinop { op; lhs = f lhs; rhs = f rhs } |> copy_mark mexp
  | AstElab.Unop { op; operand } ->
    AstElab.Unop { op; operand = f operand } |> copy_mark mexp
  | AstElab.Call { name; args } ->
    if lookup ctx name
    then AstElab.Call { name; args = List.map ~f args } |> copy_mark mexp
    else (
      let newname = Symbol.symbol ("_c0_" ^ Symbol.name name) in
      AstElab.Call { name = newname; args = List.map ~f args } |> copy_mark mexp)
  | _ -> mexp

and rename_mstm (ctx : Symbol.Set.t) (mstm : AstElab.mstm) : AstElab.mstm =
  let f = rename_mexpr ctx in
  let s = rename_mstm ctx in
  let stm = Mark.data mstm in
  match stm with
  | AstElab.Declare { var; typ; assign; body } ->
    AstElab.Declare { var; typ; assign = Option.map ~f assign; body = s body }
    |> copy_mark mstm
  | AstElab.Assign { var; exp } -> AstElab.Assign { var; exp = f exp } |> copy_mark mstm
  | AstElab.If { cond; lb; rb } ->
    AstElab.If { cond = f cond; lb = s lb; rb = s rb } |> copy_mark mstm
  | AstElab.While { cond; body } ->
    AstElab.While { cond = f cond; body = s body } |> copy_mark mstm
  | AstElab.Return r -> AstElab.Return (Option.map ~f r) |> copy_mark mstm
  | AstElab.Seq (s1, s2) -> AstElab.Seq (s s1, s s2) |> copy_mark mstm
  | AstElab.NakedExpr e -> AstElab.NakedExpr (f e) |> copy_mark mstm
  | AstElab.NakedCall { name; args } ->
    if lookup ctx name
    then AstElab.NakedCall { name; args = List.map ~f args } |> copy_mark mstm
    else (
      let newname = Symbol.symbol ("_c0_" ^ Symbol.name name) in
      AstElab.NakedCall { name = newname; args = List.map ~f args } |> copy_mark mstm)
  | _ -> mstm
;;

let rename_glob (ctx : Symbol.Set.t) (mglob : AstElab.mglob) : AstElab.mglob =
  let glob = Mark.data mglob in
  match glob with
  | AstElab.Typedef _ -> mglob
  | AstElab.Fundecl { f; args; fsig } ->
    if lookup ctx f (* if lookup succes then do not rename *)
    then mglob
    else (
      (* rename the function declaration*)
      let newname = Symbol.symbol ("_c0_" ^ Symbol.name f) in
      Mark.map ~f:(fun _ -> AstElab.Fundecl { f = newname; args; fsig }) mglob)
  | AstElab.Fundef { f; args; fsig; fdef } ->
    if lookup ctx f (* if lookup succes then do not rename *)
    then mglob
    else (
      (* rename the function declaration*)
      let newname = Symbol.symbol ("_c0_" ^ Symbol.name f) in
      Mark.map
        ~f:(fun _ ->
          AstElab.Fundef { f = newname; args; fsig; fdef = rename_mstm ctx fdef })
        mglob)
;;

let rename (header : AstElab.program) (source : AstElab.program) : AstElab.program =
  let get_ext_names (mglob : AstElab.mglob) =
    match Mark.data mglob with
    | AstElab.Fundef _ -> failwith ("Found FunDef at " ^ AstElab.Print.pp_mglob mglob)
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
