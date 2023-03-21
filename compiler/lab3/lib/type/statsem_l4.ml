open Core
module T = Ctype_l4
module SM = Symbol.Map
module SS = Symbol.Set
module A = Aste_l4
module A' = Asts
module TM = T.Map

exception TypeError

type global_ctx =
  { fdef : SS.t (*_ Delta function defined *)
  ; fdec : (T.fsig_real * int SM.t) SM.t (*_ Delta function declared with fsig *)
  ; tdef : T.t TM.t (*_ Omega type definition: tau -> t *)
  ; sdec : SS.t
  ; sdef : (T.ssig_real * int SM.t) SM.t
  }

type stm_ctx =
  { fdec : (T.fsig_real * int SM.t) SM.t (*_ Delta function defined *)
  ; tdef : T.t TM.t
  ; vdef : SS.t (*_ little delta *)
  ; vdec : (T.t * int) SM.t (*_ gamma *)
  ; sdec : SS.t
  ; sdef : (T.ssig_real * int SM.t) SM.t
  }

type exp_ctx =
  { fdec : (T.fsig_real * int SM.t) SM.t
  ; vdef : SS.t
  ; vdec : (T.t * int) SM.t
  ; sdec : SS.t
  ; sdef : (T.ssig_real * int SM.t) SM.t
  }

let f_declared fdec (f : Symbol.t) : T.fsig_real option = SM.find fdec f
let f_defined fdef (f : Symbol.t) = SS.mem fdef f
let t_defined tdef (t : T.tau) = TM.find tdef t
let validate_args tdef (args : Symbol.t list) : unit = failwith "not implemented"
let resolve (omega : T.t TM.t) (tau : T.tau) : T.t = failwith "not implemented"
let resolve_fsig (omega : T.t TM.t) (fsig : T.fsig) : T.fsig_real =
  failwith "not implemented"
;;

let is_addr : T.t -> bool = function
  | T.Star _ -> true
  | T.Any -> true
  | T.Array _ -> true
  | _ -> false
;;

(*_ type related functions *)
let small_type_size : T.t -> int = function
  | T.Int -> 4
  | T.Bool -> 4
  | T.Star _ -> 8
  | T.Array _ -> 8
  | T.Any -> 8
  | T.Struct _ -> failwith "smart_type func encounters large types"
;;

let large_type_size (sdef : T.ssig_real SM.t) (s : Symbol.t) =
  let () = failwith "struct offset problem not resolved" in
  let ssig = SM.find_exn sdef s in
  let fmax = List.fold ssig ~init:0 ~f:(fun acc (_, t) -> Int.max acc (small_type_size t)) in
  let fold_f (sm, off) (f, t) =
    let off' = off + small_type_size t in
    SM.add_exn sm ~key:f ~data:off', off'
  in
  List.fold ssig ~init:(SM.empty, 0) ~f:fold_f
;;

let rec type_unify (t1 : T.t) (t2 : T.t) : bool =
  match t1, t2 with
  | T.Any, _ -> is_addr t2
  | _, T.Any -> is_addr t1
  | T.Star t1', T.Star t2' -> type_unify t1' t2'
  | T.Array t1', T.Array t2' -> type_unify t1' t2'
  | _ -> T.equal (T.RealTyp t1) (T.RealTyp t2)
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

let static_all_func_used (prog : A.program) : SS.t = failwith "not implemented"
let static_semantic ~(hdr : A.program) ~(src : A.program) = failwith "not implemented"
