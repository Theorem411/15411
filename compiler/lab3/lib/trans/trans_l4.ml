open Core
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
    let cond : T.cond = { cmp = T.Neq; p1 = ec; p2 = T.Const (Int32.of_int_exn 0), 4 } in
    let finisher = T.If { cond; lt = l1; lf = l2 } in
    let acc, b = update acc b ~finisher ~newlab:l1 in
    (*_ tr lb *)
    let el, acc, b = tr_exp_rev env lb acc b in
    let b = { b with code = T.MovPureExp { dest = t; src = el } :: b.code } in
    let acc, b = update acc b ~finisher:(T.Goto l3) ~newlab:l2 in
    (*_ tr rb *)
    let er, acc, b = tr_exp_rev env rb acc b in
    let b = { b with code = T.MovPureExp { dest = t; src = er } :: b.code } in
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
          T.MovEfktExp { dest = t; ebop = intop_to_ebop op; lhs = el; rhs = er } :: b.code
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
    let b =
      { b with code = T.MovFuncApp { dest = Some t; fname = name; args = es } :: b.code }
    in
    (T.Temp t, esize), acc, b
  | A.Deref (A.Ptr { start; off }) ->
    let e, acc, b = tr_exp_rev env start acc_rev block_rev in
    let t = Temp.create () in
    let b =
      { b with
        code =
          T.MovPureExp { dest = t; src = T.Mem (T.Ptr { start = e; off }), esize }
          :: b.code
      }
    in
    (T.Temp t, esize), acc, b
  | A.Deref A.Null ->
    let t = Temp.create () in
    let b =
      { b with
        code =
          T.MovPureExp { dest = t; src = T.Mem (T.Ptr { start = e; off }), esize }
          :: b.code
      }
    in
    (T.Temp t, esize), acc, b
  | A.ArrayAccess aaddr -> failwith "no"
  | A.StructAccess paddr -> failwith "no"
  | A.Alloc i -> failwith "no"
  | A.Alloc_array { type_size; len } -> failwith "no"
  | A.PtrAddr paddr -> failwith "no"
  | A.ArrAddr aaddr -> failwith "no"
;;

type tr_stm_t = T.block list * T.block

let rec tr_stm_rev
  (env : Temp.t S.t)
  (stm : A.stm)
  (acc_rev : T.block list)
  (block_rev : T.block)
  : tr_stm_t
  =
  failwith "not implemented"
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
let translate (prog : A.program) : T.program = failwith "not implemented"
