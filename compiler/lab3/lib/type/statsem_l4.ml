open Core
module T = Ctype_l4
module SM = Symbol.Map
module SS = Symbol.Set
module SH = Hashtbl.Make (Symbol.T)
module A = Aste_l4
module A' = Asts
module TM = T.Map

exception TypeError

(* let print_set_ulan str set = prerr_endline (sprintf "%s: {%s}\n" str (String.concat ~sep:"," (List.map ~f:Symbol.name (SS.to_list set))));; *)

type struct_t =
  { f_offset : int SM.t
  ; tot_size : int
  ; align : int
  }

type global_ctx =
  { fdef : SS.t (*_ Delta function defined *)
  ; fdec : T.fsig_real SM.t (*_ Delta function declared with fsig *)
  ; tdef : T.t SM.t (*_ Omega type definition: tau -> t *)
  ; sdec : SS.t
  ; sdef : T.ssig_real SM.t
  ; suse : struct_t SH.t
  }

type stm_ctx =
  { fdec : T.fsig_real SM.t (*_ Delta function defined *)
  ; tdef : T.t SM.t
  ; vdef : SS.t (*_ little delta *)
  ; vdec : (T.t * int) SM.t (*_ gamma *)
  ; sdec : SS.t
  ; sdef : T.ssig_real SM.t
  ; suse : struct_t SH.t
  }

type stm_res =
  { vdef : SS.t
  ; res : A'.stm
  ; used : SS.t
  }

type exp_ctx =
  { fdec : T.fsig_real SM.t
  ; tdef : T.t SM.t
  ; vdef : SS.t
  ; vdec : (T.t * int) SM.t
  ; sdec : SS.t (*_ declared struct names *)
  ; sdef : T.ssig_real SM.t (*_ defined struct names with resolved field types *)
  ; suse : struct_t SH.t (*_ defined struct with field names explicitly calculated *)
  }

type exp_res =
  { res : A'.mexp
  ; typ : T.t
  ; used : SS.t
  }

(*_ helper: some global namespaces fetch or fail functions *)
let f_declared fdec (f : Symbol.t) : T.fsig_real option = SM.find fdec f
let f_defined fdef (f : Symbol.t) = SS.mem fdef f
let t_defined tdef (s : Symbol.t) = SM.find tdef s
(* let s_declared sdec (s : Symbol.t) = SS.find sdec ~f:(Symbol.equal s)
let s_defined sdef (s : Symbol.t) = SM.find sdef s *)

(*_ type, size, align related helper *)
let small_type_size : T.t -> int = function
  | T.Int -> 4
  | T.Bool -> 4
  | T.Star _ -> 8
  | T.Array _ -> 8
  | T.Any -> 8
  | T.Struct _ -> failwith "smart_type func encounters large types"
;;

let type_size suse (typ : T.t) =
  match typ with
  | T.Struct s ->
    let { tot_size; _ } = SH.find_exn suse s in
    tot_size
  | _ -> small_type_size typ
;;

(* let pp_suse (suse : struct_t SH.t) : string = 
  let pp_sinfo ({ f_offset; tot_size; align } : struct_t) =
    sprintf
      "{\n%s\ntot:%s\nmax:%s\n}"
      (SM.to_alist ~key_order:`Increasing f_offset
      |> List.map ~f:(fun (s, i) -> sprintf "%s:%s" (Symbol.name s) (Int.to_string i))
      |> String.concat ~sep:"\n")
      (Int.to_string tot_size)
      (Int.to_string align)
  in
    (sprintf
      "suse = \n%s\n"
      (List.map
         ~f:(fun (s, sinfo) -> sprintf "%s=%s" (Symbol.name s) (pp_sinfo sinfo))
         (SH.to_alist suse)
      |> String.concat ~sep:"\n"))
;; *)
(*_ given a struct signature (either declared or undeclared), as long as all
   the field's struct types are defined, can calculate struct size, max align, all field offsets*)
let struct_info (sdef : struct_t SH.t) (ssig : T.ssig_real) =
  (* let () = prerr_endline (sprintf "ssig_real: %s\nsdef: %s\n" (T._ssig_real_tostring ssig) (pp_suse sdef)) in *)
  let size : T.t -> int = function
    | T.Struct s ->
      let { tot_size; _ } = SH.find_exn sdef s in
      tot_size
    | t -> small_type_size t
  in
  let align : T.t -> int = function
    | T.Struct s ->
      let { align; _ } = SH.find_exn sdef s in
      align
    | t -> small_type_size t
  in
  let fold_f ((sm, accum, offset) : int SM.t * int * int) ((f, t) : Symbol.t * T.t) =
    (* let () = prerr_endline (sprintf "{%s}, accum=%s, offset=%s\n" (SM.to_alist sm |> List.map ~f:(fun (s, i) -> sprintf "%s:%s" (Symbol.name s) (Int.to_string i)) |> String.concat ~sep:", ") (Int.to_string accum) (Int.to_string offset)) in *)
    let sub = accum % align t in
    let accum' = accum + sub + size t in
    let offset' = accum' + sub in
    let sm' = SM.add_exn sm ~key:f ~data:offset in
    (* let () = prerr_endline (sprintf "{%s}, align=%s, accum'=%s, offset'=%s\n------\n" (SM.to_alist sm' |> List.map ~f:(fun (s, i) -> sprintf "%s:%s" (Symbol.name s) (Int.to_string i)) |> String.concat ~sep:", ") (Int.to_string (align t)) (Int.to_string accum') (Int.to_string offset')) in *)
    sm', accum', offset'
  in
  let max_align = List.fold ssig ~init:1 ~f:(fun acc (_, t) -> Int.max acc (align t)) in
  let f_offset, accum, _ = List.fold ssig ~init:(SM.empty, 0, 0) ~f:fold_f in
  let tot_size = accum + (accum % max_align) in
  { f_offset; tot_size; align = max_align }
;;

let struct_t_ufind (sdef : T.ssig_real SM.t) (suse : struct_t SH.t) (s : Symbol.t) =
  match SH.find suse s with
  | Some sinfo -> sinfo
  | None ->
    let ssig = SM.find_exn sdef s in
    let sinfo = struct_info suse ssig in
    let () = SH.update suse s ~f:(fun _ -> sinfo) in
    sinfo
;;

(*_ helper: function args validation
    1. function arg names are unique 
    2. arg names are not type names 
    3. arg names are not struct names
    4. arg types are all small types  *)
let validate_args tdef (args : Symbol.t list) ((ats, rett) : T.fsig_real) : unit =
  let chk (arg, t) =
    if SM.mem tdef arg
    then
      raise TypeError
      (* else if SM.mem sdec arg
    then raise TypeError *)
    else if not (Int.equal (List.length args) (SS.length (SS.of_list args)))
    then raise TypeError
    else (
      let (_ : int) = small_type_size t in
      ())
  in
  let check_list = List.zip_exn args ats in
  let () =
    match rett with
    | None -> ()
    | Some t ->
      let (_ : int) = small_type_size t in
      ()
  in
  List.iter check_list ~f:chk
;;

(*_ resolve named types and return set of struct names implicitly used *)
let rec resolve tdef tau =
  match tau with
  | T.RealTyp t -> t, SS.empty
  | T.FakeTyp s -> SM.find_exn tdef s, SS.empty
  | T.Star tau' ->
    let tres, sres = resolve tdef tau' in
    T.Star tres, sres
  | T.Array tau' ->
    let tres, sres = resolve tdef tau' in
    T.Array tres, sres
  | T.Struct s -> T.Struct s, SS.singleton s
;;

let resolve_fsig (tdef : T.t SM.t) (fsig : T.fsig) : T.fsig_real * SS.t =
  let argstyp, rettyp = fsig in
  let rettyp', sname_ret =
    match rettyp with
    | None -> None, SS.empty
    | Some tau ->
      let t, sname_ret = resolve tdef tau in
      Some t, sname_ret
  in
  let argstyp', sname =
    List.map argstyp ~f:(fun tau ->
      let t, snames = resolve tdef tau in
      t, snames)
    |> List.unzip
  in
  (argstyp', rettyp'), SS.union_list sname |> SS.union sname_ret
;;

let resolve_ssig (tdef : T.t SM.t) (ssig : T.ssig) : T.ssig_real * SS.t =
  let map_f (s, tau) =
    let t, ss = resolve tdef tau in
    (s, t), ss
  in
  let ssig_real, snames = List.map ssig ~f:map_f |> List.unzip in
  ssig_real, SS.union_list snames
;;

let struct_in_field (ssig : T.ssig_real) : SS.t =
  let structs =
    List.filter_map ssig ~f:(fun (_, t) ->
      match t with
      | T.Struct s -> Some s
      | _ -> None)
  in
  SS.of_list structs
;;

let struct_cyclic_chk (sdec : SS.t) (sdef : T.ssig_real SM.t) : unit =
  let vs : SS.t = SS.union sdec (SM.key_set sdef) in
  let es = SM.of_key_set vs ~f:(fun _ -> SS.empty) in
  let update ~key ~data acc =
    let flds = struct_in_field data in
    SM.update acc key ~f:(fun _ -> flds)
  in
  let es = SM.fold sdef ~init:es ~f:update in
  let vinit = SS.empty in
  let rec dfs (v : Symbol.t) (visited : SS.t) : SS.t =
    if SS.mem visited v
    then raise TypeError
    else (
      let visited' = SS.add visited v in
      let nbrs = SM.find_exn es v in
      SS.fold ~init:visited' nbrs ~f:(fun acc u -> dfs u acc))
  in
  let (_ : SS.t) = SS.fold vs ~init:vinit ~f:(fun acc v -> dfs v acc) in
  ()
;;

(*_ helper: extended type equation for polymorphic compare op *)
let is_ptraddr_exn : T.t -> unit = function
  | T.Star _ -> ()
  | T.Any -> ()
  | _ -> raise TypeError
;;

let rec type_unify (t1 : T.t) (t2 : T.t) : bool =
  let is_addr = function
    | T.Any | T.Star _ -> true
    | _ -> false
  in
  match t1, t2 with
  | T.Any, _ -> is_addr t2
  | _, T.Any -> is_addr t1
  | T.Star t1', T.Star t2' -> type_unify t1' t2'
  | T.Array t1', T.Array t2' -> type_unify t1' t2'
  | _ -> T.equal (T.RealTyp t1) (T.RealTyp t2)
;;

let type_unify_exn t1 t2 = if type_unify t1 t2 then () else raise TypeError

(*_ main static_semantics checks
  1. static_semantic_gdecl : check global level declarations and definitions 
  2. static_semantic_stmt : check stmt level expected return types, collect initialized variables, and return transformed ast'
  3. static_semantic_exp :
  4. static_semantic_exp_chk : *)
let rec static_semantic_exp (mexp : A.mexp) (exp_ctx : exp_ctx) : exp_res =
  match Mark.data mexp with
  | A.True -> { res = A'.True, 4; typ = T.Bool; used = SS.empty }
  | A.False -> { res = A'.False, 4; typ = T.Bool; used = SS.empty }
  | A.Var x ->
    if SS.mem exp_ctx.vdef x
    then (
      match SM.find exp_ctx.vdec x with
      | None -> raise TypeError
      (* | Some (typ, i) -> { res = A'.Var x, i; typ; used = SS.singleton x }) *)
      | Some (typ, i) -> { res = A'.Var x, i; typ; used = SS.empty })
    else raise TypeError
  | A.Const n -> { res = A'.Const n, 4; typ = T.Int; used = SS.empty }
  | A.Ternary { cond; lb; rb } ->
    let { res = cond'; typ = condt; used = condu } = static_semantic_exp cond exp_ctx in
    let { res = lb'; typ = lt; used = lu } = static_semantic_exp lb exp_ctx in
    let { res = rb'; typ = rt; used = ru } = static_semantic_exp rb exp_ctx in
    type_unify_exn lt rt;
    type_unify_exn condt T.Bool;
    { res = A'.Ternary { cond = cond'; lb = lb'; rb = rb' }, small_type_size lt
    ; typ = lt
    ; used = SS.union_list [ lu; ru; condu ]
    }
  | A.PureBinop { lhs; rhs; op } ->
    let { res = lhs'; typ = tl; used = lu } = static_semantic_exp lhs exp_ctx in
    let { res = rhs'; typ = tr; used = ru } = static_semantic_exp rhs exp_ctx in
    type_unify_exn tl T.Int;
    type_unify_exn tr T.Int;
    { res = A'.PureBinop { op = A'.intop_pure op; lhs = lhs'; rhs = rhs' }, 4
    ; typ = T.Int
    ; used = SS.union lu ru
    }
  | A.EfktBinop { lhs; rhs; op } ->
    let { res = lhs'; typ = tl; used = lu } = static_semantic_exp lhs exp_ctx in
    let { res = rhs'; typ = tr; used = ru } = static_semantic_exp rhs exp_ctx in
    type_unify_exn tl T.Int;
    type_unify_exn tr T.Int;
    { res = A'.EfktBinop { op = A'.intop_efkt op; lhs = lhs'; rhs = rhs' }, 4
    ; typ = T.Int
    ; used = SS.union lu ru
    }
  | A.CmpBinop { op; lhs; rhs } ->
    let { res = lhs'; typ = tl; used = lu } = static_semantic_exp lhs exp_ctx in
    let { res = rhs'; typ = tr; used = ru } = static_semantic_exp rhs exp_ctx in
    let used = SS.union lu ru in
    (match small_type_size tl, small_type_size tr with
     | 4, 4 ->
       let () =
         match op with
         | A.Leq | A.Less | A.Geq | A.Greater ->
           type_unify_exn tl T.Int;
           type_unify_exn tr T.Int
         | _ -> ()
       in
       type_unify_exn tl tr;
       { res = A'.CmpBinop { op = A'.intop_cmp op; lhs = lhs'; rhs = rhs'; size = 4 }, 4
       ; typ = T.Bool
       ; used
       }
     | 8, 8 ->
       type_unify_exn tl tr;
       is_ptraddr_exn tl;
       is_ptraddr_exn tr;
       { res = A'.CmpBinop { op = A'.ptrop_cmp op; lhs = lhs'; rhs = rhs'; size = 8 }, 4
       ; typ = T.Bool
       ; used
       }
     | _ -> raise TypeError)
  | A.Unop { op; operand } ->
    let { res; typ; used } = static_semantic_exp operand exp_ctx in
    (match op with
     | A.LogNot ->
       type_unify_exn typ T.Bool;
       { res = A'.Unop { op = A'.unop op; operand = res }, 4; typ = T.Bool; used }
     | A.BitNot ->
       type_unify_exn typ T.Int;
       { res = A'.Unop { op = A'.unop op; operand = res }, 4; typ = T.Int; used })
  | A.Call { name; args } ->
    (*_ check function name: not variable names *)
    let () = if SM.mem exp_ctx.vdec name then raise TypeError else () in
    (*_ check function signature: return type not none *)
    let tlist, rettyp = SM.find_exn exp_ctx.fdec name in
    let rett =
      match rettyp with
      | None -> raise TypeError
      | Some t -> t
    in
    (*_ check function signature: argument types are correct *)
    let checklist = List.zip_exn args tlist in
    let checkfun (e, t) =
      let { res; typ; used } = static_semantic_exp e exp_ctx in
      type_unify_exn t typ;
      res, used
    in
    let args, used = List.map checklist ~f:checkfun |> List.unzip in
    { res = A'.Call { name; args }, small_type_size rett
    ; typ = rett
    ; used = SS.add (SS.union_list used) name
    }
  | A.Null -> { res = A'.PtrAddr A'.Null, 8; typ = T.Any; used = SS.empty }
  | A.Deref e ->
    let { res; typ; used } = static_semantic_exp e exp_ctx in
    let paddr = A'.Ptr { start = res; off = 0 } in
    (match typ with
     | T.Star t -> { res = A'.Deref paddr, type_size exp_ctx.suse t; typ = t; used }
     | _ -> raise TypeError)
  | A.ArrAccess { arr; idx } ->
    let { res = rarr; typ = tarr; used = uarr } = static_semantic_exp arr exp_ctx in
    let { res = ridx; typ = ti; used = ui } = static_semantic_exp idx exp_ctx in
    let typ =
      match tarr with
      | T.Array t -> t
      | _ -> raise TypeError
    in
    let size = type_size exp_ctx.suse typ in
    type_unify_exn ti T.Int;
    { res = A'.ArrayAccess { head = rarr; idx = ridx; size; extra = 0 }, size
    ; typ
    ; used = SS.union uarr ui
    }
  | A.StructDot { str; field } ->
    let { res = sres, _; typ = styp; used } = static_semantic_exp str exp_ctx in
    let s =
      match styp with
      | T.Struct s -> s
      | _ -> raise TypeError
    in
    let { f_offset; _ } = struct_t_ufind exp_ctx.sdef exp_ctx.suse s in
    let ssig = SM.find_exn exp_ctx.sdef s in
    let ssig' = SM.of_alist_exn ssig in
    let typ = SM.find_exn ssig' field in
    let i = SM.find_exn f_offset field in
    let res =
      match sres with
      | A'.Deref (A'.Ptr { start; off }) ->
        A'.StructAccess (A'.Ptr { start; off = off + i })
      | A'.ArrayAccess { head; idx; size; extra } ->
        A'.ArrayAccess { head; idx; size; extra = extra + i }
      | A'.StructAccess (A'.Ptr {start; off}) ->
        A'.StructAccess (A'.Ptr {start; off=off+i})
      | _ -> failwith "statsem: structDot encounters incorrect lvalue"
    in
    { res = res, type_size exp_ctx.suse typ; typ; used }
  | A.StructArr { str; field } ->
    let { res = sres; typ = styp; used } = static_semantic_exp str exp_ctx in
    let s =
      match styp with
      | T.Star (T.Struct s) -> s
      | _ -> raise TypeError
    in
    let { f_offset; _ } = struct_t_ufind exp_ctx.sdef exp_ctx.suse s in
    let ssig = SM.find_exn exp_ctx.sdef s in
    let ssig' = SM.of_alist_exn ssig in
    let typ = SM.find_exn ssig' field in
    let i = SM.find_exn f_offset field in
    let res = A'.StructAccess (A'.Ptr { start = sres; off = i }) in
    { res = res, type_size exp_ctx.suse typ; typ; used }
  | A.Alloc tau ->
    let t, snames = resolve exp_ctx.tdef tau in
    let () =
      if SS.is_subset snames ~of_:(SM.key_set exp_ctx.sdef) then () else raise TypeError
    in
    let size =
      match t with
      | T.Struct s ->
        (match SH.find exp_ctx.suse s with
         | None ->
           let ssig = SM.find_exn exp_ctx.sdef s in
           let struct_info = struct_info exp_ctx.suse ssig in
           SH.update exp_ctx.suse s ~f:(fun _ -> struct_info);
           struct_info.tot_size
         | Some { tot_size; _ } -> tot_size)
      | _ -> small_type_size t
    in
    { res = A'.Alloc size, 8; typ = T.Star t; used = SS.empty }
  | A.Alloc_array { typ; len } ->
    let t, snames = resolve exp_ctx.tdef typ in
    let () =
      if SS.is_subset snames ~of_:(SM.key_set exp_ctx.sdef) then () else raise TypeError
    in
    let { res = lres; used; _ } = static_semantic_exp len exp_ctx in
    let type_size =
      match t with
      | T.Struct s ->
        (match SH.find exp_ctx.suse s with
         | None ->
           let ssig = SM.find_exn exp_ctx.sdef s in
           let struct_info = struct_info exp_ctx.suse ssig in
           SH.update exp_ctx.suse s ~f:(fun _ -> struct_info);
           struct_info.tot_size
         | Some { tot_size; _ } -> tot_size)
      | _ -> small_type_size t
    in
    { res = A'.Alloc_array { type_size; len = lres }, 8; typ = T.Array t; used }
;;

(* let not_type_names (tdef : T.t SM.t) (s : Symbol.t) =
  match t_defined tdef s with
  | Some _ -> raise TypeError
  | None -> ()
;; *)

(* let not_func_names (fdec : SS.t) s =
  match SS.find fdec ~f:(Symbol.equal s) with
  | Some _ -> raise TypeError
  | None -> ()
;; *)

let not_declared_yet (vdec : (T.t * int) SM.t) (s : Symbol.t) =
  match SM.find vdec s with
  | Some _ -> raise TypeError
  | None -> ()
;;

(* 
let not_struct_names (sdec : SS.t) (s : Symbol.t) =
  match SS.find sdec ~f:(Symbol.equal s) with
  | Some _ -> raise TypeError
  | None -> ()
;; *)

let rec static_semantic_stm
  (mstm : A.mstm)
  ({ fdec; tdef; vdef; vdec; sdec; sdef; suse } : stm_ctx)
  (topt : T.t option)
  : stm_res
  =
  match Mark.data mstm with
  | A.Declare { var; typ; assign; body } ->
    let t, snames = resolve tdef typ in
    (*_ var is not type names, not redeclared, not struct names*)
    let () = not_type_names tdef var in
    let () = not_declared_yet vdec var in
    let size = small_type_size t in
    let vdec' = SM.add_exn vdec ~key:var ~data:(t, size) in
    (match assign with
     | None ->
       let { vdef = vdef'; res = body; used = us } =
         static_semantic_stm
           body
           { fdec; tdef; vdef; vdec = vdec'; sdec = SS.union sdec snames; sdef; suse }
           topt
       in
       { vdef = SS.remove vdef' var
       ; res = A'.Declare { var; assign = None; body }
       ; used = us
       }
     | Some ae ->
       let { res = eres; typ; used = ue } =
         static_semantic_exp ae { fdec; tdef; vdef; vdec; sdec; sdef; suse }
       in
       let { vdef = vdef'; res = body; used = us } =
         static_semantic_stm
           body
           { fdec
           ; tdef
           ; vdef = SS.add vdef var
           ; vdec = vdec'
           ; sdec = SS.union sdec snames
           ; sdef
           ; suse
           }
           topt
       in
       type_unify_exn t typ;
       { vdef = SS.remove vdef' var
       ; res = A'.Declare { var; assign = Some eres; body }
       ; used = SS.union us ue
       })
  | A.Asop { dest; op = Some o; exp } ->
    let stm_ctx = { fdec; tdef; vdef; vdec; sdec; sdef; suse } in
    let { res = rd, _; typ = td; used = ud } = static_semantic_exp dest stm_ctx in
    let { res = re; typ = te; used = ue } = static_semantic_exp exp stm_ctx in
    let dest = Symbol.create_fresh () in
    (*_ depends on the shape of rd, do differently *)
    let lvalue =
      match rd with
      | A'.Deref ptraddr -> A'.PtrAddr ptraddr
      | A'.ArrayAccess arraddr -> A'.ArrAddr arraddr
      | A'.StructAccess ptraddr -> A'.PtrAddr ptraddr
      | _ -> failwith "incorrect lvalue shape for in statsem"
    in
    let res =
      A'.Declare
        { var = dest
        ; assign = Some (lvalue, 8)
        ; body = A'.AssignMem { dest; op = Some (A'.intop o); exp = re }
        }
    in
    type_unify_exn td T.Int;
    type_unify_exn te T.Int;
    { vdef; res; used = SS.union ud ue }
  | A.Asop { dest; op = None; exp } ->
    let stm_ctx = { fdec; tdef; vdef; vdec; sdec; sdef; suse } in
    let { res = rd, _; typ = td; used = ud } = static_semantic_exp dest stm_ctx in
    let { res = re; typ = te; used = ue } = static_semantic_exp exp stm_ctx in
    let dest = Symbol.create_fresh () in
    (* let () = printf ">>> rd=%s" (A'.Print.pp_exp rd) in *)
    let lvalue =
      match rd with
      | A'.Deref ptraddr -> A'.PtrAddr ptraddr
      | A'.ArrayAccess arraddr -> A'.ArrAddr arraddr
      | A'.StructAccess ptraddr -> A'.PtrAddr ptraddr
      | _ -> failwith "incorrect lvalue shape for in statsem"
    in
    let res =
      A'.Declare
        { var = dest
        ; assign = Some (lvalue, 8)
        ; body = A'.AssignMem { dest; op = None; exp = re }
        }
    in
    type_unify_exn td te;
    { vdef; res; used = SS.union ud ue }
  | A.Assign { var; exp } ->
    let t, _ = SM.find_exn vdec var in
    let { res; typ; used } =
      static_semantic_exp exp { fdec; tdef; vdef; vdec; sdec; sdef; suse }
    in
    (* printf ">>> t=%s; typ=%s\n" (T._t_tostring t) (T._t_tostring typ); *)
    type_unify_exn t typ;
    { vdef = SS.add vdef var; res = A'.Assign { var; exp = res }; used }
  | A.If { cond; lb; rb } ->
    let { vdef = vdef1; res = r1; used = u1 } =
      static_semantic_stm lb { fdec; tdef; vdef; vdec; sdec; sdef; suse } topt
    in
    let { vdef = vdef2; res = r2; used = u2 } =
      static_semantic_stm rb { fdec; tdef; vdef; vdec; sdec; sdef; suse } topt
    in
    let { res = resc; typ = tc; used = uc } =
      static_semantic_exp cond { fdec; vdef; vdec; tdef; sdec; sdef; suse }
    in
    type_unify_exn tc T.Bool;
    { vdef = SS.inter vdef1 vdef2
    ; res = A'.If { cond = resc; lb = r1; rb = r2 }
    ; used = SS.union_list [ u1; u2; uc ]
    }
  | A.While { cond; body } ->
    let { vdef = _; res = rs; used = ue } =
      static_semantic_stm body { fdec; tdef; vdef; vdec; sdec; sdef; suse } topt
    in
    let { res = re; typ; used = us } =
      static_semantic_exp cond { fdec; tdef; vdef; vdec; sdec; sdef; suse }
    in
    type_unify_exn typ T.Bool;
    { vdef; res = A'.While { cond = re; body = rs }; used = SS.union ue us }
  | A.Return eopt ->
    let vdef' = SM.key_set vdec in
    (match topt, eopt with
     | Some t, Some e ->
       let { res; typ; used } =
         static_semantic_exp e { fdec; tdef; vdec; vdef; sdec; sdef; suse }
       in
       type_unify_exn typ t;
       { vdef = vdef'; res = A'.Return (Some res); used }
     | None, None -> { vdef = vdef'; res = A'.Return None; used = SS.empty }
     | _ -> raise TypeError)
  | A.Nop -> { vdef; res = A'.Nop; used = SS.empty }
  | A.Seq (s1, s2) ->
    let { vdef = vdef1; res = res1; used = used1 } =
      static_semantic_stm s1 { fdec; tdef; vdef; vdec; sdec; sdef; suse } topt
    in
    let { vdef = vdef2; res = res2; used = used2 } =
      static_semantic_stm s2 { fdec; tdef; vdef = vdef1; vdec; sdec; sdef; suse } topt
    in
    { vdef = vdef2; res = A'.Seq (res1, res2); used = SS.union used1 used2 }
  | A.NakedExpr e ->
    let { res; used; _ } =
      static_semantic_exp e { fdec; tdef; vdec; vdef; sdec; sdef; suse }
    in
    { vdef; res = A'.NakedExpr res; used }
  | A.AssertFail -> { vdef; res = A'.AssertFail; used = SS.empty }
  | A.NakedCall { name; args } ->
    (*_ check function name: not variable names *)
    let () = if SM.mem vdec name then raise TypeError else () in
    (*_ check function signature: return type not none *)
    let tlist, _ = SM.find_exn fdec name in
    (*_ check function signature: argument types are correct *)
    let checklist = List.zip_exn args tlist in
    let checkfun (e, t) =
      let { res; typ; used } =
        static_semantic_exp e { fdec; tdef; vdec; vdef; sdec; sdef; suse }
      in
      type_unify_exn t typ;
      res, used
    in
    let args, used = List.map checklist ~f:checkfun |> List.unzip in
    { vdef; res = A'.NakedCall { name; args }; used = SS.add (SS.union_list used) name }
;;

(*_ resolve tau to t and collect all implicitly defined struct types
   this is essentially a fold function, 
   return new global_ctx and used function names *)
let static_semantic_gdecl
  (gdecl : A.mglob)
  ({ fdef; fdec; tdef; sdec; sdef; suse } : global_ctx)
  : global_ctx * SS.t * A'.glob option
  =
  match Mark.data gdecl with
  | A.Typedef { told; tnew } ->
    if SS.mem fdef tnew
    then raise TypeError
    else if SM.mem fdec tnew
    then raise TypeError
    else if SM.mem tdef tnew
    then raise TypeError
    else (
      let t, snames = resolve tdef told in
      ( { fdef
        ; fdec
        ; tdef = SM.add_exn tdef ~key:tnew ~data:t
        ; sdec = SS.union sdec snames
        ; sdef
        ; suse
        }
      , SS.empty
      , None ))
  | A.Fundecl { f; fsig; args } ->
    let fsig_real, snames = resolve_fsig tdef fsig in
    let () = validate_args tdef args fsig_real in
    (match f_declared fdec f with
     | None ->
       (*_ if f hasn't been declared yet, check: f not type name *)
       let () =
         match t_defined tdef f with
         | None -> ()
         | Some _ -> raise TypeError
       in
       let fdec' = SM.add_exn fdec ~key:f ~data:fsig_real in
       ( { fdef; fdec = fdec'; tdef; sdec = SS.union sdec snames; sdef; suse }
       , SS.empty
       , None )
     | Some fsig_real' ->
       (*_ if f is redeclared: check fsig is the same *)
       if T.equal_fsig_real fsig_real fsig_real'
       then { fdef; fdec; tdef; sdec = SS.union sdec snames; sdef; suse }, SS.empty, None
       else raise TypeError)
  | A.Fundef { f; args; fsig; fdef = mstm } ->
    let fsig_real, snames = resolve_fsig tdef fsig in
    let () =
      match t_defined tdef f with
      | None -> ()
      | Some _ -> raise TypeError
    in
    (*_ validate args *)
    let () = validate_args tdef args fsig_real in
    (*_ f is not redefined *)
    let () = if f_defined fdef f then raise TypeError else () in
    (*_ if f is declared, check fsig equality *)
    let () =
      match SM.find fdec f with
      | None -> ()
      | Some fsig_real' ->
        if T.equal_fsig_real fsig_real fsig_real' then () else raise TypeError
    in
    (*_ update fdef and fdec *)
    let fdef' = SS.add fdef f in
    let fdec' =
      match SM.add fdec ~key:f ~data:fsig_real with
      | `Ok map -> map
      | `Duplicate -> fdec
    in
    let tlist, topt = fsig_real in
    let args_size =
      List.zip_exn args tlist |> List.map ~f:(fun (s, t) -> s, (t, small_type_size t))
    in
    let stm_ctx : stm_ctx =
      { tdef
      ; fdec = fdec'
      ; vdef = SS.of_list args
      ; vdec = SM.of_alist_exn args_size
      ; sdec = SS.union sdec snames
      ; sdef
      ; suse
      }
    in
    let ({ res; used; _ } : stm_res) = static_semantic_stm mstm stm_ctx topt in
    ( { fdef = fdef'; fdec = fdec'; tdef; sdec = SS.union sdec snames; sdef; suse }
    , used
    , Some { f; args = List.map args_size ~f:(fun (x, (_, i)) -> x, i); fdef = res } )
  | A.Sdecl s ->
    (* not_func_names (SM.key_set fdec) s;
    not_type_names tdef s; *)
    { fdef; fdec; tdef; sdec = SS.add sdec s; sdef; suse }, SS.empty, None
  | A.Sdef { sname; ssig } ->
    let ssig_real, sname_implicit = resolve_ssig tdef ssig in
    (* let () = printf ">>> ssig_real = %s\n" (T._ssig_real_tostring ssig_real) in *)
    (* let () = struct_cyclic_chk sdef sname ssig_real in *)
    let sdef' = SM.add_exn sdef ~key:sname ~data:ssig_real in
    (* not_func_names (SM.key_set fdec) sname;
    not_type_names tdef sname; *)
    ( { fdef
      ; fdec
      ; tdef
      ; sdec = SS.add (SS.union sdec sname_implicit) sname
      ; sdef = sdef'
      ; suse
      }
    , SS.empty
    , None )
;;

(*_ mli goal *)
let static_semantic ~(hdr : A.program) ~(src : A.program) : A'.program =
  let global_ctx_init =
    { fdef = SS.empty
    ; fdec = SM.empty
    ; tdef = SM.empty
    ; sdec = SS.empty
    ; sdef = SM.empty
    ; suse = SH.create ()
    }
  in
  let fold_f (global_ctx, used) glob =
    let global_ctx', used', _ = static_semantic_gdecl glob global_ctx in
    global_ctx', SS.union used used'
  in
  let global_ctx_hdr, used = List.fold hdr ~init:(global_ctx_init, SS.empty) ~f:fold_f in
  let () = if SS.length global_ctx_hdr.fdef = 0 then () else raise TypeError in
  (*_ for src file assume all function declared in hdr is defined *)
  let global_ctx_init' =
    { fdef = SM.key_set global_ctx_hdr.fdec
    ; fdec = global_ctx_hdr.fdec
    ; tdef = global_ctx_hdr.tdef
    ; sdec = global_ctx_hdr.sdec
    ; sdef = global_ctx_hdr.sdef
    ; suse = global_ctx_hdr.suse
    }
  in
  let fold_f (global_ctx, used, prog) glob =
    let global_ctx', used', glob_opt = static_semantic_gdecl glob global_ctx in
    let prog' =
      match glob_opt with
      | None -> prog
      | Some glob -> glob :: prog
    in
    global_ctx', SS.union used used', prog'
  in
  let global_ctx_src, used', program =
    List.fold src ~init:(global_ctx_init', used, []) ~f:fold_f
  in
  let program = List.rev program in
  (*_ main function is defined *)
  let () =
    if SS.mem global_ctx_src.fdef (Symbol.symbol "main") then () else raise TypeError
  in
  (* let () = print_set_ulan "globalfdef" global_ctx_src.fdef in  *)
  (*_ all used functions are declared *)
  let () = if SS.is_subset used' ~of_:global_ctx_src.fdef then () else raise TypeError in
  (*_ struct defs are not cyclic *)
  let () = struct_cyclic_chk global_ctx_src.sdec global_ctx_src.sdef in
  (*_ prrint fstruct offsets and stuff *)
  (* let () = prerr_endline (pp_suse global_ctx_src.suse) in *)
  program
;;
