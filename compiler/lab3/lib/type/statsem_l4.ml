open Core
module T = Ctype_l4
module SM = Symbol.Map
module SS = Symbol.Set
module A = Aste_l4
module A' = Asts
module TM = T.Map

exception TypeError

type struct_t =
  { ssig : T.ssig_real
  ; f_offset : int SM.t
  ; tot_size : int
  ; align : int
  }

type global_ctx =
  { fdef : SS.t (*_ Delta function defined *)
  ; fdec : T.fsig_real SM.t (*_ Delta function declared with fsig *)
  ; tdef : T.t TM.t (*_ Omega type definition: tau -> t *)
  ; sdec : SS.t
  ; sdef : struct_t SM.t
  }

type stm_ctx =
  { fdec : T.fsig_real SM.t (*_ Delta function defined *)
  ; tdef : T.t TM.t
  ; vdef : SS.t (*_ little delta *)
  ; vdec : (T.t * int) SM.t (*_ gamma *)
  ; sdec : SS.t
  ; sdef : struct_t SM.t
  }

type stm_res =
  { vdec : (T.t * int) SM.t
  ; res : A'.stm
  ; used : SS.t
  }

type exp_ctx =
  { fdec : T.fsig_real SM.t
  ; vdef : SS.t
  ; vdec : (T.t * int) SM.t
  ; sdec : SS.t
  ; sdef : struct_t SM.t
  }
type exp_res = 
  { res : A'.exp 
  ; used : SS.t}

(*_ helper: some global namespaces fetch or fail functions *)
let f_declared fdec (f : Symbol.t) : T.fsig_real option = SM.find fdec f
let f_defined fdef (f : Symbol.t) = SS.mem fdef f
let t_defined tdef (t : T.tau) = TM.find tdef t
let s_declared sdec (s : Symbol.t) = SS.find sdec ~f:(Symbol.equal s)
let s_defined sdef (s : Symbol.t) = SM.find sdef s

(*_ type, size, align related helper *)
let small_type_size : T.t -> int = function
  | T.Int -> 4
  | T.Bool -> 4
  | T.Star _ -> 8
  | T.Array _ -> 8
  | T.Any -> 8
  | T.Struct _ -> failwith "smart_type func encounters large types"
;;

(*_ given a struct signature (either declared or undeclared), as long as all
   the field's struct types are defined, can calculate struct size, max align, all field offsets*)
let struct_info (sdef : struct_t SM.t) (ssig : T.ssig_real) =
  let size_align : T.t -> int * int = function
    | T.Struct s ->
      let { tot_size; align; _ } = SM.find_exn sdef s in
      tot_size, align
    | t ->
      let i = small_type_size t in
      i, i
  in
  let fold_f ((sm, accum, offset) : int SM.t * int * int) ((f, t) : Symbol.t * T.t) =
    let size, align = size_align t in
    let sub = accum % align in
    let offset' = accum + sub in
    let accum' = accum + sub + size in
    SM.add_exn sm ~key:f ~data:offset, accum', offset'
  in
  let max_align = List.fold ssig ~init:0 ~f:(fun acc (_, t) -> Int.max acc (align t)) in
  let f_offset, accum, _ = List.fold ssig ~init:(SM.empty, 0, 0) ~f:fold_f in
  let tot_size = accum + (accum % max_align) in
  { ssig; f_offset; tot_size; align = max_align }
;;

(*_ helper: function args validation
    1. function arg names are unique 
    2. arg names are not type names 
    3. arg names are not struct names
    4. arg types are all small types  *)
let validate_args tdef (args : Symbol.t list) ((ats, _) : T.fsig_real) : unit =
  let chk (arg, t) =
    if TM.mem tdef (T.FakeTyp arg)
    then raise TypeError
    else if TM.mem tdef (T.Struct arg)
    then raise TypeError
    else if not (Int.equal (List.length args) (SS.length (SS.of_list args)))
    then raise TypeError
    else (
      let (_ : int) = small_type_size t in
      ())
  in
  let check_list = List.zip_exn args ats in
  List.iter check_list ~f:chk
;;

let resolve (omega : T.t TM.t) (tau : T.tau) : T.t =
  match TM.find omega tau with
  | None -> raise TypeError
  | Some t -> t
;;

let resolve_fsig (omega : T.t TM.t) (fsig : T.fsig) : T.fsig_real =
  let argstyp, rettyp = fsig in
  let rettyp' =
    match rettyp with
    | None -> None
    | Some tau -> Some (resolve omega tau)
  in
  let argstyp' = List.map argstyp ~f:(resolve omega) in
  argstyp', rettyp'
;;

(*_ helper: extended type equation for polymorphic compare op *)
let is_addr : T.t -> bool = function
  | T.Star _ -> true
  | T.Any -> true
  | T.Array _ -> true
  | _ -> false
;;

let rec type_unify (t1 : T.t) (t2 : T.t) : bool =
  match t1, t2 with
  | T.Any, _ -> is_addr t2
  | _, T.Any -> is_addr t1
  | T.Star t1', T.Star t2' -> type_unify t1' t2'
  | T.Array t1', T.Array t2' -> type_unify t1' t2'
  | _ -> T.equal (T.RealTyp t1) (T.RealTyp t2)
;;

(*_ main static_semantics checks
  1. static_semantic_gdecl : check global level declarations and definitions 
  2. static_semantic_stmt : check stmt level expected return types, collect initialized variables, and return transformed ast'
  3. static_semantic_exp_syn :
  4. static_semantic_exp_chk : *)
let rec static_semantic_exp_syn
  (mexp : A.mexp)
  ({ fdec; vdef; vdec; sdec; sdef } : exp_ctx)
  : T.t
  =
  failwith "not implemented"

and static_semantic_exp_chk
  (mexp : A.mexp)
  ({ fdec; vdef; vdec; sdec; sdef } : exp_ctx)
  (t : T.t)
  =
  let t' = static_semantic_exp_syn mexp { fdec; vdef; vdec; sdec; sdef } in
  if type_unify t t' then () else raise TypeError
;;

let static_semantic_stm
  (mstm : A.mstm)
  ({ fdec; tdef; vdef; vdec; sdec; sdef } : stm_ctx)
  (topt : T.t option)
  : stm_res
  =
  match Mark.data mstm with
  | A.Declare { var; typ; assign; body } -> failwith "not implemented"
  | A.Asop { dest; op; exp } -> failwith "not implemented"
  | A.Assign { var; exp } -> failwith "not implemented"
  | A.If { cond; lb; rb } -> failwith "not implemented"
  | A.While { cond; body } -> failwith "not implemented"
  | A.Return eopt -> failwith "not implemented"
  | A.Nop -> failwith "not implemented"
  | A.Seq (s1, s2) -> failwith "not implemented"
  | A.NakedExpr e -> failwith "not implemented"
  | A.AssertFail -> failwith "not implemented"
  | A.NakedCall { name; args } -> failwith "not implemented"
;;

let static_semantic_gdecl
  (gdecl : A.mglob)
  ({ fdef; fdec; tdef; sdec; sdef } : global_ctx)
  : global_ctx
  =
  match Mark.data gdecl with
  | A.Typedef { told; tnew } -> failwith "not implemented"
  | A.Fundecl { f; fsig; args } -> failwith "not implemented"
  | A.Fundef { f; args; fsig; fdef = mstm } -> failwith "not implemented"
  | A.Sdecl s -> failwith "not implemented"
  | A.Sdef { sname; ssig } -> failwith "not implemented"
;;
(*_ mli goal *)
let static_semantic ~(hdr : A.program) ~(src : A.program) = failwith "not implemented"
