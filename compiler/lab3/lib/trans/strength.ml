open Core
module T = Tree_l4

(*_ strength reduction idea for binop: 
   
*)

let rec tr_traversal (tree : T.mpexp) (op : T.pbop) : T.mpexp list =
  (*_ return top-level operator, and break as munch sub-components as possible *)
  let exp, _ = tree in
  match exp with
  | T.Binop { op = op'; lhs; rhs } ->
    if T.equal_pbop op op' then tr_traversal lhs op @ tr_traversal rhs op else [ tree ]
  | _ -> [ tree ]
;;

let rec tr_assemble (exp_lst : T.mpexp list) (op : T.pbop) : T.mpexp =
  match exp_lst with
  | [] -> failwith "strength.ml: something is wrong"
  | [ e ] -> e
  | lhs :: es -> T.Binop { op; lhs; rhs = tr_assemble es op }, 4
;;

let const_sieve (exp_lst : T.mpexp list) : Int32.t list * T.mpexp list =
  let f (e, _) =
    match e with
    | T.Const _ -> true
    | _ -> false
  in
  let clst, elst = List.partition_tf exp_lst ~f in
  let clst' =
    List.map clst ~f:(fun (e, _) ->
        match e with
        | T.Const n -> n
        | _ -> failwith "strength.ml: then why did u say ur constant???")
  in
  clst', elst
;;

let pbop_int32op (op : T.pbop) =
  match op with
  | T.Add -> Int32.( + )
  | T.Mul -> Int32.( * )
  | T.Sub -> Int32.( - )
  | T.BitAnd -> Int32.bit_and
  | T.BitOr -> Int32.bit_or
  | T.BitXor -> Int32.bit_xor
;;

let fold_const (clst : Int32.t list) (op : T.pbop) : Int32.t option =
  (* Why not just use a reduce? *)
  (*_ Tricky! Check with init values for different op; and use fold_left!! *)
  let init =
    match op with
    | T.Add -> 0
    | T.Mul -> 1
    | T.Sub -> failwith "strength.ml: subtraction cannot apply associativity optimization"
    | T.BitAnd -> -1
    | T.BitOr -> 0
    | T.BitXor -> 0
  in
  let init = Int32.of_int_exn init in
  let op' = pbop_int32op op in
  (* List.reduce_balanced clst ~f:op' *)
  match clst with
  | [] -> None
  | _ -> Some (List.fold_left ~init clst ~f:op')
;;

let sr_assoc_binop (const : Int32.t) (elst : T.mpexp list) (op : T.pbop) : T.mpexp =
  (* prerr_endline
    (sprintf
       "Called sr_assoc_binop with const=%d, elst = [%s] and some op"
       (Int.of_int32_exn const)
       (String.concat ~sep:"," (List.map elst ~f:T.Print.pp_mpexp))); *)
  let rhs = tr_assemble elst op in
  let zero = Int32.of_int_exn 0 in
  let minus = Int32.of_int_exn (-1) in
  let one = Int32.of_int_exn 1 in
  match op with
  | T.Add ->
    if Int32.equal const zero then rhs else T.Binop { lhs = T.Const const, 4; rhs; op }, 4
  | T.Mul ->
    if Int32.equal const zero
    then T.Const zero, 4
    else if Int32.equal const one
    then rhs
    else T.Binop { lhs = T.Const const, 4; rhs; op }, 4
  | T.BitAnd ->
    if Int32.equal const minus
    then rhs
    else T.Binop { lhs = T.Const const, 4; rhs; op }, 4
  | T.BitOr ->
    if Int32.equal const zero then rhs else T.Binop { lhs = T.Const const, 4; rhs; op }, 4
  | T.BitXor ->
    if Int32.equal const zero then rhs else T.Binop { lhs = T.Const const, 4; rhs; op }, 4
  | T.Sub -> failwith "strength.ml: subtraction cannot apply associativity optimization"
;;

let sr_cmpop (lhs : T.mpexp) (rhs : T.mpexp) (op : T.cbop) : T.pexp =
  let texp = T.Const (Int32.of_int_exn 1) in
  let fexp = T.Const (Int32.of_int_exn 0) in
  match lhs, rhs with
  | (T.Const nl, _), (T.Const nr, _) ->
    (match op with
    | T.Eq -> if Int32.equal nl nr then texp else fexp
    | T.Neq -> if not (Int32.equal nl nr) then texp else fexp
    | T.Geq -> if Int32.( >= ) nl nr then texp else fexp
    | T.Greater -> if Int32.( > ) nl nr then texp else fexp
    | T.Leq -> if Int32.( <= ) nl nr then texp else fexp
    | T.Less -> if Int32.( < ) nl nr then texp else fexp)
  | _ -> T.Cmpop { lhs; rhs; op; size = 4 }
;;

let rec sr_mpexp (mexp : T.mpexp) : T.mpexp =
  let exp, esize = mexp in
  match exp with
  | Binop { lhs; rhs; op = T.Sub } ->
    let el, sl = sr_mpexp lhs in
    let er, sr = sr_mpexp rhs in
    (match el, er with
    | T.Const nl, T.Const nr -> T.Const (Int32.( - ) nl nr), esize
    | _ -> T.Binop { lhs = el, sl; rhs = er, sr; op = T.Sub }, esize)
  | Binop { lhs; rhs; op } ->
    (*_ associativity optimizations *)
    let lhs' = sr_mpexp lhs in
    let rhs' = sr_mpexp rhs in
    let exp_lst = tr_traversal (T.Binop { lhs = lhs'; rhs = rhs'; op }, esize) op in
    let clst, elst = const_sieve exp_lst in
    (match fold_const clst op, elst with
    | None, _ -> T.Binop { lhs = lhs'; rhs = rhs'; op }, esize
    | Some c, [] -> T.Const c, esize
    | Some c, _ -> sr_assoc_binop c elst op)
  | Cmpop { lhs; rhs; op; size = 4 } ->
    let lhs' = sr_mpexp lhs in
    let rhs' = sr_mpexp rhs in
    sr_cmpop lhs' rhs' op, esize
  | Unop { p; op } ->
    let exp, esize = p in
    let bitnot =
      match op with
      | T.BitNot -> Int32.bit_not
    in
    (match exp with
    | T.Const n -> T.Const (bitnot n), esize
    | _ -> mexp)
  | Mem (Ptr paddr) ->
    let paddr' : T.ptraddr = { paddr with start = sr_mpexp paddr.start } in
    let addr' : T.addr = T.Ptr paddr' in
    T.Mem addr', esize
  | Addr (Ptr paddr) ->
    let paddr' : T.ptraddr = { paddr with start = sr_mpexp paddr.start } in
    let addr' : T.addr = T.Ptr paddr' in
    T.Addr addr', esize
  | Mem (Arr aaddr) ->
    let aaddr' : T.arraddr =
      { aaddr with head = sr_mpexp aaddr.head; idx = sr_mpexp aaddr.idx }
    in
    let addr' : T.addr = T.Arr aaddr' in
    T.Mem addr', esize
  | Addr (Arr aaddr) ->
    let aaddr' : T.arraddr =
      { aaddr with head = sr_mpexp aaddr.head; idx = sr_mpexp aaddr.idx }
    in
    let addr' : T.addr = T.Arr aaddr' in
    T.Addr addr', esize
  | _ -> mexp
;;

let sr_stm (stm : T.stm) : T.stm =
  match stm with
  | If ifz ->
    let cond' : T.cond =
      match ifz.cond with
      | LCond { p1; p2; cmp } ->
        let p1', p2' = sr_mpexp p1, sr_mpexp p2 in
        LCond { p1 = p1'; p2 = p2'; cmp }
      | SCond { p1; p2; cmp } ->
        let p1', p2' = sr_mpexp p1, sr_mpexp p2 in
        SCond { p1 = p1'; p2 = p2'; cmp }
    in
    If { ifz with cond = cond' }
  | MovEfktExp efkt ->
    let lhs', rhs' = sr_mpexp efkt.lhs, sr_mpexp efkt.rhs in
    MovEfktExp { efkt with lhs = lhs'; rhs = rhs' }
  | MovPureExp pure ->
    let src' = sr_mpexp pure.src in
    MovPureExp { pure with src = src' }
  | MovFuncApp fapp ->
    let args' = List.map fapp.args ~f:sr_mpexp in
    MovFuncApp { fapp with args = args' }
  | Calloc calloc ->
    let len' = sr_mpexp calloc.len in
    Calloc { calloc with len = len' }
  | MovToMem { addr = Ptr ptraddr; src } ->
    let src' = sr_mpexp src in
    let addr' = T.Ptr { ptraddr with start = sr_mpexp ptraddr.start } in
    MovToMem { addr = addr'; src = src' }
  | MovToMem { addr = Arr arraddr; src } ->
    let src' = sr_mpexp src in
    let addr' =
      T.Arr { arraddr with head = sr_mpexp arraddr.head; idx = sr_mpexp arraddr.idx }
    in
    MovToMem { addr = addr'; src = src' }
  | Return (Some e) ->
    let e' = sr_mpexp e in
    Return (Some e')
  | _ -> stm
;;

let sr_block (block : T.block) : T.block =
  { block with block = List.map block.block ~f:sr_stm }
;;

let sr_fspace (fspace : T.fspace_block) : T.fspace_block =
  { fspace with fdef = List.map fspace.fdef ~f:sr_block }
;;

let strength_reduction (prog : T.program) : T.program = List.map prog ~f:sr_fspace