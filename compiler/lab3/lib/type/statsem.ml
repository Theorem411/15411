open Core
module T = Ctype
module SM = Symbol.Map
module SS = Symbol.Set
module A = Aste
module TM = T.Map

(*_ exception and error printing *)
(* let global_err = Error_msg.create () *)

exception TypeError

(*_ global ctx and local ctx *)
type global_ctx =
  { fdef : SS.t (*_ Delta function defined *)
  ; fdec : T.fsig_real SM.t (*_ Delta function declared with fsig *)
  ; tdef : T.t TM.t (*_ Omega type definition: tau -> t *)
  }

type stm_ctx =
  { fdec : T.fsig_real SM.t (*_ Delta function defined *)
  ; tdef : T.t TM.t
  ; vdef : SS.t (*_ little delta *)
  ; vdec : T.t SM.t (*_ gamma *)
  }

type exp_ctx =
  { fdec : T.fsig_real SM.t
  ; vdef : SS.t
  ; vdec : T.t SM.t
  }

let f_declared fdec (f : Symbol.t) : T.fsig_real option = SM.find fdec f
let f_defined fdef (f : Symbol.t) = SS.mem fdef f
let t_defined tdef (t : T.tau) = TM.find tdef t
(* let v_declared vdec (v : Temp.t) : T.t option = Temp.Map.find vdec v
let v_defined vdef (v : Temp.t) : bool = Temp.Set.mem vdef v *)

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

(*_ static_semantic_exp *)
let rec static_semantic_exp_syn (mexp : A.mexp) ({ fdec; vdef; vdec } : exp_ctx) : T.t =
  match Mark.data mexp with
  | A.True -> T.Bool
  | A.False -> T.Bool
  | A.Var x ->
    if SS.mem vdef x
    then (
      match SM.find vdec x with
      | None -> raise TypeError
      | Some t -> t)
    else raise TypeError
  | A.Const _ -> T.Int
  | A.Ternary { cond; lb; rb } ->
    let t = static_semantic_exp_syn lb { fdec; vdef; vdec } in
    static_semantic_exp_chk rb { fdec; vdef; vdec } t;
    static_semantic_exp_chk cond { fdec; vdef; vdec } T.Bool;
    t
  | A.PureBinop { lhs; rhs; _ } | A.EfktBinop { lhs; rhs; _ } ->
    static_semantic_exp_chk lhs { fdec; vdef; vdec } T.Int;
    static_semantic_exp_chk rhs { fdec; vdef; vdec } T.Int;
    T.Int
  | A.CmpBinop { op; lhs; rhs } ->
    let () =
      match op with
      | A.Leq | A.Less | A.Geq | A.Greater ->
        static_semantic_exp_chk lhs { fdec; vdef; vdec } T.Int;
        static_semantic_exp_chk rhs { fdec; vdef; vdec } T.Int;
        ()
      | A.Eq | A.Neq ->
        let t = static_semantic_exp_syn lhs { fdec; vdef; vdec } in
        static_semantic_exp_chk rhs { fdec; vdef; vdec } t
    in
    T.Bool
  | A.Unop { op; operand } ->
    (match op with
     | LogNot ->
       static_semantic_exp_chk operand { fdec; vdef; vdec } T.Bool;
       T.Bool
     | BitNot ->
       static_semantic_exp_chk operand { fdec; vdef; vdec } T.Int;
       T.Int)
  | A.Call { name; args } ->
    let () = if SM.mem vdec name then raise TypeError else () in
    let () = print_string (Symbol.name name ^ "\n") in
    let () = print_string (Symbol.pp_sm vdec ~f:(T._t_tostring) ^ "\n") in
    let tlist, rettyp = SM.find_exn fdec name in
    let checklist = List.zip_exn args tlist in
    let checkfun (e, t) = static_semantic_exp_chk e { fdec; vdef; vdec } t in
    let () = List.iter checklist ~f:checkfun in
    (match rettyp with
     | None -> raise TypeError
     | Some t -> t)

and static_semantic_exp_chk (mexp : A.mexp) ({ fdec; vdef; vdec } : exp_ctx) (t : T.t) =
  let t' = static_semantic_exp_syn mexp { fdec; vdef; vdec } in
  if T.equal (T.RealTyp t) (T.RealTyp t') then () else raise TypeError
;;

(*_ static_semantic_stm *)
let rec static_semantic_stm
  (mstm : A.mstm)
  ({ fdec; tdef; vdef; vdec } : stm_ctx)
  (topt : T.t option)
  : SS.t
  =
  match Mark.data mstm with
  | A.Declare { var; typ; assign; body } ->
    let t = resolve tdef typ in
    let () =
      match t_defined tdef (T.FakeTyp var) with
      | Some _ -> raise TypeError
      | None -> ()
    in
    let () =
      match SM.find vdec var with
      | Some _ -> raise TypeError
      | None -> ()
    in
    let vdec' = SM.add_exn vdec ~key:var ~data:t in
    (match assign with
     | None ->
       let vdef' = static_semantic_stm body { fdec; tdef; vdec = vdec'; vdef } topt in
       SS.remove vdef' var
     | Some ae ->
       let () = static_semantic_exp_chk ae { fdec; vdec; vdef } t in
       let vdef' =
         static_semantic_stm
           body
           { fdec; tdef; vdec = vdec'; vdef = SS.add vdef var }
           topt
       in
       SS.remove vdef' var)
  | A.Assign { var; exp } ->
    let t = SM.find_exn vdec var in
    let () = static_semantic_exp_chk exp { fdec; vdef; vdec } t in
    SS.add vdef var
  | A.If { cond; lb; rb } ->
    let vdef1 = static_semantic_stm lb { fdec; tdef; vdef; vdec } topt in
    let vdef2 = static_semantic_stm rb { fdec; tdef; vdef; vdec } topt in
    let () = static_semantic_exp_chk cond { fdec; vdef; vdec } T.Bool in
    SS.inter vdef1 vdef2
  | A.While { cond; body } ->
    let (_ : SS.t) = static_semantic_stm body { fdec; tdef; vdef; vdec } topt in
    let () = static_semantic_exp_chk cond { fdec; vdef; vdec } T.Bool in
    vdef
  | A.Return eopt ->
    let () =
      match topt, eopt with
      | Some t, Some e -> static_semantic_exp_chk e { fdec; vdec; vdef } t
      | None, None -> ()
      | _ -> raise TypeError
    in
    SM.key_set vdec
  | A.Nop -> vdef
  | A.Seq (s1, s2) ->
    let vdef' = static_semantic_stm s1 { fdec; tdef; vdef; vdec } topt in
    let vdef'' = static_semantic_stm s2 { fdec; tdef; vdef = vdef'; vdec } topt in
    vdef''
  | A.NakedExpr e ->
    let (_ : T.t) = static_semantic_exp_syn e { fdec; vdef; vdec } in
    vdef
  | A.AssertFail -> vdef
  | A.NakedCall { name; args } ->
    let () = if SM.mem vdec name then raise TypeError else () in
    let tlist, _ = SM.find_exn fdec name in
    let checklist = List.zip_exn args tlist in
    let checkfun (e, t) = static_semantic_exp_chk e { fdec; vdef; vdec } t in
    let () = List.iter checklist ~f:checkfun in
    vdef
;;

(*_ static_semantic_gdecl *)
let static_semantic_gdecl (gdecl : A.mglob) ({ fdef; fdec; tdef } : global_ctx)
  : global_ctx
  =
  match Mark.data gdecl with
  | A.Typedef { told; tnew } ->
    let tnew_sym =
      match tnew with
      | T.RealTyp _ -> raise TypeError
      | T.FakeTyp t -> t
    in
    let t = resolve tdef told in
    if SS.mem fdef tnew_sym
    then raise TypeError
    else if TM.mem tdef tnew
    then raise TypeError
    else { fdef; fdec; tdef = TM.add_exn tdef ~key:tnew ~data:t }
  | A.Fundecl { f; fsig; _ } ->
    let fsig_real = resolve_fsig tdef fsig in
    (* let () = printf "%s\n\n" (Symbol.pp_sm fdec ~f:T._fsig_real_tostring) in *)
    (* let () = print_string (T._fsig_real_tostring (fsig_real) ^ "\n") in *)
    (match f_declared fdec f with
     | None ->
       let () =
         match t_defined tdef (T.FakeTyp f) with
         | None -> ()
         | Some _ -> raise TypeError
       in
       let fdec' = SM.add_exn fdec ~key:f ~data:fsig_real in
       { fdef; fdec = fdec'; tdef }
     | Some fsig_real' ->
       if T.equal_fsig_real fsig_real fsig_real'
       then { fdef; fdec; tdef }
       else raise TypeError)
  | A.Fundef { f; args; fsig; fdef = mstm } ->
    let fsig_real = resolve_fsig tdef fsig in
    let () =
      match t_defined tdef (T.FakeTyp f) with
      | None -> ()
      | Some _ -> raise TypeError
    in
    let () = if f_defined fdef f then () else () in
    let fdef' = SS.add fdef f in
    let fdec' =
      match SM.add fdec ~key:f ~data:fsig_real with
      | `Ok map -> map
      | `Duplicate -> fdec
    in
    let tlist, topt = fsig_real in
    let vdec = List.zip_exn args tlist |> SM.of_alist_exn in
    let stm_ctx = { tdef; fdec = fdec'; vdef = SS.of_list args; vdec } in
    let (_ : SS.t) = static_semantic_stm mstm stm_ctx topt in
    { fdef = fdef'; fdec = fdec'; tdef }
;;

let all_func_used (prog : A.program) : SS.t =
  let rec all_func_used_exp (mexp : A.mexp) =
    match Mark.data mexp with
    | A.True | A.False | A.Var _ | A.Const _ -> SS.empty
    | A.Ternary { cond; lb; rb } ->
      [ all_func_used_exp cond; all_func_used_exp lb; all_func_used_exp rb ]
      |> List.fold ~init:SS.empty ~f:SS.union
    | A.PureBinop { lhs; rhs; _ }
    | A.EfktBinop { lhs; rhs; _ }
    | A.CmpBinop { lhs; rhs; _ } ->
      SS.union (all_func_used_exp lhs) (all_func_used_exp rhs)
    | A.Unop { operand; _ } -> all_func_used_exp operand
    | A.Call { name; args } ->
      List.map args ~f:all_func_used_exp
      |> List.fold ~init:(SS.singleton name) ~f:SS.union
  in
  let rec all_func_used_stm (mstm : A.mstm) =
    match Mark.data mstm with
    | A.Declare { body; assign; _ } ->
      (match assign with
       | None -> all_func_used_stm body
       | Some e -> SS.union (all_func_used_exp e) (all_func_used_stm body))
    | A.Assign { exp; _ } -> all_func_used_exp exp
    | A.If { cond; lb; rb } ->
      [ all_func_used_exp cond; all_func_used_stm lb; all_func_used_stm rb ]
      |> List.fold ~init:SS.empty ~f:SS.union
    | A.While { cond; body } -> SS.union (all_func_used_exp cond) (all_func_used_stm body)
    | A.Return eopt ->
      (match eopt with
       | None -> SS.empty
       | Some e -> all_func_used_exp e)
    | A.Nop -> SS.empty
    | A.Seq (s1, s2) -> SS.union (all_func_used_stm s1) (all_func_used_stm s2)
    | A.NakedExpr e -> all_func_used_exp e
    | A.AssertFail -> SS.empty
    | A.NakedCall { name; args } ->
      List.map args ~f:all_func_used_exp
      |> List.fold ~init:(SS.singleton name) ~f:SS.union
  in
  let all_func_used_gdecl (mglob : A.mglob) =
    match Mark.data mglob with
    | A.Fundef { fdef; _ } -> all_func_used_stm fdef
    | _ -> SS.empty
  in
  let res_init = SS.empty in
  let fold_gdecl acc gdecl = SS.union acc (all_func_used_gdecl gdecl) in
  List.fold prog ~init:res_init ~f:fold_gdecl
;;

(*_ do static_semantic_gdecl on hdr and produce global_ctx 
    check global_ctx doesn't have defined 
    create a new global_ctx that: move all declared f in global_ctx to defined 
    run static_semantic_gdecl on src and produce new global_ctx
    do a pass to return all the used functions
    check whether this used set is all defined 
    *)
let static_semantic ~(hdr : A.program) ~(src : A.program) =
  let tdef_init =
    TM.empty
    |> TM.add_exn ~key:(T.RealTyp T.Int) ~data:T.Int
    |> TM.add_exn ~key:(T.RealTyp T.Bool) ~data:T.Bool
  in
  let global_ctx_init = { fdef = SS.empty; fdec = SM.empty; tdef = tdef_init } in
  let fold_f global_ctx mglob = static_semantic_gdecl mglob global_ctx in
  let global_ctx_hdr = List.fold hdr ~init:global_ctx_init ~f:fold_f in
  let () = if SS.length global_ctx_hdr.fdef = 0 then () else raise TypeError in
  let global_ctx_init' =
    { fdef =
        SM.key_set global_ctx_hdr.fdec (*_ f declared in hdr are considered defined *)
    ; fdec = global_ctx_hdr.fdec
    ; tdef = global_ctx_hdr.tdef
    }
  in
  let global_ctx_src = List.fold src ~init:global_ctx_init' ~f:fold_f in
  let used_funcs = all_func_used src in
  let () =
    if SS.is_subset used_funcs ~of_:global_ctx_src.fdef then () else raise TypeError
  in
  ()
;;
