(* open Core
module A = Asts
module T = Tree_l4
module S = Symbol.Map

let intop_to_pbop = function
  | A.Plus -> T.Add
  | A.Minus -> T.Sub
  | A.Times -> T.Mul
  | A.BitAnd -> T.BitAnd
  | A.BitOr -> T.BitOr
  | A.BitXor -> T.BitXor
;;

let intop_to_cbop = function
  | A.Eq -> T.Eq
  | A.Neq -> T.Neq
  | A.Leq -> T.Leq
  | A.Less -> T.Less
  | A.Geq -> T.Geq
  | A.Greater -> T.Greater
;;

let intop_to_ebop = function
  | A.Divided_by -> T.Div
  | A.Modulo -> T.Mod
  | A.ShiftL -> T.ShftL
  | A.ShiftR -> T.ShftR
;;

(*_ block handling *)
(*_ given A.stm list in rev order, produce a block *)
type block_tobe =
  { l : Label.t
  ; code : T.stm list
  }

let to_block ({ l; code } : block_tobe) ~(finisher : T.stm) : T.block =
  let jtag =
    match finisher with
    | T.Return _ -> T.Ret
    | T.If { lt; lf; _ } -> T.Cond { lt; lf }
    | T.Goto l -> T.Uncon l
    | _ -> failwith "ur kidding right?"
  in
  { label = l; block = List.rev (finisher :: code); jump = jtag }
;;

let update acc b ~(finisher : T.stm) ~(newlab : Label.t) =
  let new_block = to_block b ~finisher in
  let acc' = new_block :: acc in
  let b' = { l = newlab; code = [] } in
  acc', b'
;;

type tr_exp_t = T.mpexp * T.block list * block_tobe

let rec tr_exp_rev
  (env : Temp.t S.t)
  (exp : A.mexp)
  (acc_rev : T.block list)
  (block_rev : block_tobe)
  : tr_exp_t
  =
  let e, esize = exp in
  match e with
  | A.True -> (T.Const (Int32.of_int_exn 1), esize), acc_rev, block_rev
  | A.False -> (T.Const (Int32.of_int_exn 0), esize), acc_rev, block_rev
  | A.Var x -> (T.Temp (S.find_exn env x), esize), acc_rev, block_rev
  | A.Const n -> (T.Const n, esize), acc_rev, block_rev
  | A.Ternary { cond; lb; rb } ->
    let t = Temp.create () in
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    (*_ tr cond *)
    let ec, acc, b = tr_exp_rev env cond acc_rev block_rev in
    let cond : T.cond =
      { cmp = T.Neq; p1 = ec; size = T.size ec; p2 = T.Const (Int32.of_int_exn 0), 4 }
    in
    let finisher = T.If { cond; lt = l1; lf = l2 } in
    let acc, b = update acc b ~finisher ~newlab:l1 in
    (*_ tr lb *)
    let el, acc, b = tr_exp_rev env lb acc b in
    let b =
      { b with code = T.MovPureExp { dest = t; size = T.size el; src = el } :: b.code }
    in
    let acc, b = update acc b ~finisher:(T.Goto l3) ~newlab:l2 in
    (*_ tr rb *)
    let er, acc, b = tr_exp_rev env rb acc b in
    let b =
      { b with code = T.MovPureExp { dest = t; size = T.size er; src = er } :: b.code }
    in
    let acc, b = update acc b ~finisher:(T.Goto l3) ~newlab:l3 in
    (T.Temp t, esize), acc, b
  | A.PureBinop { op; lhs; rhs } ->
    let el, acc, b = tr_exp_rev env lhs acc_rev block_rev in
    let er, acc, b = tr_exp_rev env rhs acc b in
    (T.Binop { op = intop_to_pbop op; lhs = el; rhs = er }, esize), acc, b
  | A.EfktBinop { op; lhs; rhs } ->
    let t = Temp.create () in
    let el, acc, b = tr_exp_rev env lhs acc_rev block_rev in
    let er, acc, b = tr_exp_rev env rhs acc b in
    let b =
      { b with
        code =
          T.MovEfktExp
            { dest = t; size = T.size el; ebop = intop_to_ebop op; lhs = el; rhs = er }
          :: b.code
      }
    in
    (T.Temp t, esize), acc, b
  | A.CmpBinop { op; size; lhs; rhs } ->
    let el, acc, b = tr_exp_rev env lhs acc_rev block_rev in
    let er, acc, b = tr_exp_rev env rhs acc b in
    (T.Cmpop { op = intop_to_cbop op; size; lhs = el; rhs = er }, esize), acc, b
  | A.Unop { op = A.LogNot; operand = A.Unop { op = A.LogNot; operand }, _ } ->
    tr_exp_rev env operand acc_rev block_rev
  | A.Unop { op = A.BitNot; operand = A.Unop { op = A.BitNot; operand }, _ } ->
    tr_exp_rev env operand acc_rev block_rev
  | A.Unop { op = A.LogNot; operand } ->
    let e, acc, b = tr_exp_rev env operand acc_rev block_rev in
    ( (T.Binop { op = T.BitXor; lhs = e; rhs = T.Const (Int32.of_int_exn 1), 4 }, esize)
    , acc
    , b )
  | A.Unop { op = A.BitNot; operand } ->
    let e, acc, b = tr_exp_rev env operand acc_rev block_rev in
    (T.Unop { op = T.BitNot; p = e }, esize), acc, b
  | A.Call { name; args } ->
    let t = Temp.create () in
    let fold_f (es, acc, b) a =
      let e, acc, b = tr_exp_rev env a acc b in
      e :: es, acc, b
    in
    let es, acc, b = List.fold args ~init:([], acc_rev, block_rev) ~f:fold_f in
    let es = List.rev es in
    let b =
      { b with
        code =
          T.MovFuncApp { dest = Some (t, esize); fname = name; args = es } :: b.code
      }
    in
    (T.Temp t, esize), acc, b
  | A.Deref (A.Ptr { start; off }) | A.StructAccess (A.Ptr { start; off }) ->
    let e, acc, b = tr_exp_rev env start acc_rev block_rev in
    let t = Temp.create () in
    let b =
      { b with
        code =
          T.MovPureExp
            { dest = t; size = T.size e; src = T.Mem (T.Ptr { start = e; off }), esize }
          :: b.code
      }
    in
    (T.Temp t, esize), acc, b
  | A.Deref A.Null | A.StructAccess A.Null ->
    let t = Temp.create () in
    let b =
      { block_rev with
        code =
          T.MovPureExp { dest = t; size = esize; src = T.Mem T.Null, esize }
          :: block_rev.code
      }
    in
    (T.Temp t, esize), acc_rev, b
  | A.ArrayAccess { head; idx; size; extra } ->
    let t = Temp.create () in
    let ehead, acc, b = tr_exp_rev env head acc_rev block_rev in
    let eidx, acc, b = tr_exp_rev env idx acc b in
    let src = T.Mem (T.Arr { head = ehead; idx = eidx; typ_size = size; extra }), esize in
    let b = { b with code = T.MovPureExp { dest = t; size = esize; src } :: b.code } in
    (T.Temp t, esize), acc, b
  | A.Alloc i -> (T.Alloc i, esize), acc_rev, block_rev
  | A.Alloc_array { type_size; len } ->
    let elen, acc, b = tr_exp_rev env len acc_rev block_rev in
    (T.Calloc { len = elen; typ = type_size }, esize), acc, b
  | A.PtrAddr (A.Ptr { start; off }) ->
    let e, acc, b = tr_exp_rev env start acc_rev block_rev in
    (T.Addr (T.Ptr { start = e; off }), esize), acc, b
  | A.PtrAddr A.Null -> (T.Addr T.Null, esize), acc_rev, block_rev
  | A.ArrAddr { head; idx; size; extra } ->
    let ehead, acc, b = tr_exp_rev env head acc_rev block_rev in
    let eidx, acc, b = tr_exp_rev env idx acc b in
    (T.Addr (T.Arr { head = ehead; idx = eidx; typ_size = size; extra }), esize), acc, b
;;

type tr_stm_t = T.block list * block_tobe

let rec tr_stm_rev
  (env : Temp.t S.t)
  (stm : A.stm)
  (acc_rev : T.block list)
  (block_rev : block_tobe)
  : tr_stm_t
  =
  match stm with
  | Declare { var; assign = None; body; _} ->
    let t = Temp.create () in
    let env' = S.set env ~key:var ~data:t in
    let acc, b = tr_stm_rev env' body acc_rev block_rev in
    acc, b
  | Declare { var; typ_size; assign = Some exp; body } ->
    let t = Temp.create () in
    let env' = S.set env ~key:var ~data:t in
    (*_ tr assign *)
    let e, acc, b = tr_exp_rev env exp acc_rev block_rev in
    (*_ tr body *)
    let acc, b = tr_stm_rev env' body acc b in
    let b =
      { b with code = T.MovPureExp { dest = t; size = typ_size; src = e } :: b.code }
    in
    acc, b
  | Assign { var; typ_size; exp } ->
    let t = S.find_exn env var in
    let e, acc, b = tr_exp_rev env exp acc_rev block_rev in
    let b =
      { b with code = T.MovPureExp { dest = t; size = typ_size; src = e } :: b.code }
    in
    acc, b
  | Asop { dest; op = None; exp } -> 
    
    failwith "no"
  | Asop { dest; op = Some (A.Pure op); exp } -> 
    failwith "no"
  | Asop { dest; op = Some (A.Efkt op); exp } -> 
    failwith "no"
  | If { cond = A.True, _; lb; _ } -> tr_stm_rev env lb acc_rev block_rev
  | If { cond = A.False, _; rb; _ } -> tr_stm_rev env rb acc_rev block_rev
  | If { cond = A.Unop { op = A.LogNot; operand }, _; lb; rb } ->
    tr_stm_rev env (If { cond = operand; rb; lb }) acc_rev block_rev
  | If { cond = A.CmpBinop { op; size; lhs; rhs }, _; lb; rb } ->
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    (*_ translate cond *)
    let el, acc, b = tr_exp_rev env lhs acc_rev block_rev in
    let er, acc, b = tr_exp_rev env rhs acc b in
    let cond : T.cond = { cmp = intop_to_cbop op; size; p1 = el; p2 = er } in
    let finisher = T.If { cond; lt = l1; lf = l2 } in
    let new_block = to_block b ~finisher in
    (*_ translate lb *)
    let b = { l = l1; code = [] } in
    let acc = new_block :: acc in
    let acc, b = tr_stm_rev env lb acc b in
    let new_block = to_block b ~finisher:(T.Goto l3) in
    (*_ translate rb *)
    let b = { l = l2; code = [] } in
    let acc = new_block :: acc in
    let acc, b = tr_stm_rev env rb acc b in
    let new_block = to_block b ~finisher:(T.Goto l3) in
    (*_ result*)
    let b = { l = l3; code = [] } in
    let acc = new_block :: acc in
    acc, b
  | If { cond; lb; rb } ->
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    (*_ translate cond *)
    let ec, acc, b = tr_exp_rev env cond acc_rev block_rev in
    let cond : T.cond =
      { cmp = T.Neq; size = T.size ec; p1 = ec; p2 = T.Const (Int32.of_int_exn 0), 4 }
    in
    let finisher = T.If { cond; lt = l1; lf = l2 } in
    let new_block = to_block b ~finisher in
    (*_ translate lb *)
    let b = { l = l1; code = [] } in
    let acc = new_block :: acc in
    let acc, b = tr_stm_rev env lb acc b in
    let new_block = to_block b ~finisher:(T.Goto l3) in
    (*_ translate rb *)
    let b = { l = l2; code = [] } in
    let acc = new_block :: acc in
    let acc, b = tr_stm_rev env rb acc b in
    let new_block = to_block b ~finisher:(T.Goto l3) in
    (*_ result*)
    let b = { l = l3; code = [] } in
    let acc = new_block :: acc in
    acc, b
  | While { cond = A.True, _; body } ->
    let l1 = Label.create () in
    let l2 = Label.create () in
    let new_block = to_block block_rev ~finisher:(T.Goto l1) in
    (*_ *)
    let b = { l = l1; code = [] } in
    let acc = new_block :: acc_rev in
    let acc, b = tr_stm_rev env body acc b in
    let new_block = to_block b ~finisher:(T.Goto l1) in
    (*_ res *)
    let acc = new_block :: acc in
    let b = { l = l2; code = [] } in
    acc, b
  | While { cond = A.False, _; body } -> tr_stm_rev env body acc_rev block_rev
  | While { cond = A.CmpBinop { op; size; lhs; rhs }, _; body } ->
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    let new_block = to_block block_rev ~finisher:(T.Goto l1) in
    let acc = new_block :: acc_rev in
    let b = { l = l1; code = [] } in
    (*_ tr cond *)
    let el, acc, b = tr_exp_rev env lhs acc b in
    let er, acc, b = tr_exp_rev env rhs acc b in
    let cond : T.cond = { cmp = intop_to_cbop op; size; p1 = el; p2 = er } in
    let finisher = T.If { cond; lt = l2; lf = l3 } in
    let new_block = to_block b ~finisher in
    let acc = new_block :: acc in
    let b = { l = l2; code = [] } in
    (*_ tr body *)
    let acc, b = tr_stm_rev env body acc b in
    let new_block = to_block b ~finisher:(T.Goto l1) in
    let acc = new_block :: acc in
    let b = { l = l3; code = [] } in
    acc, b
  | While { cond; body } ->
    let l1 = Label.create () in
    let l2 = Label.create () in
    let l3 = Label.create () in
    let new_block = to_block block_rev ~finisher:(T.Goto l1) in
    let acc = new_block :: acc_rev in
    let b = { l = l1; code = [] } in
    (*_ tr cond *)
    let ec, acc, b = tr_exp_rev env cond acc b in
    let cond : T.cond =
      { cmp = T.Neq; p1 = ec; size = T.size ec; p2 = T.Const (Int32.of_int_exn 0), 4 }
    in
    let finisher = T.If { cond; lt = l2; lf = l3 } in
    let new_block = to_block b ~finisher in
    let acc = new_block :: acc in
    let b = { l = l2; code = [] } in
    (*_ tr body *)
    let acc, b = tr_stm_rev env body acc b in
    let new_block = to_block b ~finisher:(T.Goto l1) in
    let acc = new_block :: acc in
    let b = { l = l3; code = [] } in
    acc, b
  | Return None ->
    let new_block = to_block block_rev ~finisher:(T.Return None) in
    let acc = new_block :: acc_rev in
    let b = { l = Label.create (); code = [] } in
    acc, b
  | Return (Some exp) ->
    let e, acc, b = tr_exp_rev env exp acc_rev block_rev in
    let new_block = to_block b ~finisher:(T.Return (Some e)) in
    let acc = new_block :: acc in
    let b = { l = Label.create (); code = [] } in
    acc, b
  | Nop -> acc_rev, block_rev
  | Seq (s1, s2) ->
    let acc, b = tr_stm_rev env s1 acc_rev block_rev in
    let acc, b = tr_stm_rev env s2 acc b in
    acc, b
  | NakedExpr exp ->
    let t = Temp.create () in
    let e, acc, b = tr_exp_rev env exp acc_rev block_rev in
    let b = { b with code = T.MovPureExp { dest = t; size=T.size e; src = e } :: b.code } in
    acc, b
  | AssertFail ->
    let b = { block_rev with code = T.AssertFail :: block_rev.code } in
    acc_rev, b
  | NakedCall { name; args } ->
    let fold_f (es, acc, b) a =
      let e, acc, b = tr_exp_rev env a acc b in
      e :: es, acc, b
    in
    let es, acc, b = List.fold args ~init:([], acc_rev, block_rev) ~f:fold_f in
    let es = List.rev es in
    let b =
      { b with code = T.MovFuncApp { dest = None; fname = name; args = es } :: b.code }
    in
    acc, b
;;

let args_tag (sl : Symbol.t list) =
  let s2t =
    List.map sl ~f:(fun s ->
      let t = Temp.create () in
      s, t)
  in
  let sset = S.of_alist_exn s2t in
  let _, tlst = List.unzip s2t in
  (* List.fold ~init:([], S.empty) ~f:(fun (args, acc) s ->
    let t = Temp.create () in
    t :: args, S.set acc ~key:s ~data:t) sl *)
  tlst, sset
;;

let tr_glob (glob : A.glob) = failwith "not implemented"
let translate (prog : A.program) : T.program = failwith "not implemented" *)
