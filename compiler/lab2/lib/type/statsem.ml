(* open Core
module T = Ctype
module SM = Symbol.Map
module SS = Symbol.Set
module A = Aste
module TM = T.Map

(*_ exception and error printing *)
let global_err = Error_msg.create ()

exception TypeError

(*_ global ctx and local ctx *)
type global_ctx = 
  { fdef : SS.t (*_ Delta function declare with fsig *)
  ; fdec : T.fsig_real SM.t (*_ Delta function defined *)
  ; tdef : T.t TM.t (*_ Omega type definition: tau -> t *)
  }

type local_ctx =
  { fdef : SS.t (*_ Delta function declare with fsig *)
  ; fdec : T.fsig_real SM.t (*_ Delta function defined *)
  ; vdef : Temp.Set.t (*_ little delta *)
  ; vdec : T.t Temp.Map.t  (*_ gamma *)
  }

let f_declared fdec (f : Symbol.t) : T.fsig_real option = SM.find fdec f
;;
let f_defined fdef (f : Symbol.t) = SS.mem fdef f
;;
let t_defined tdef (t : T.tau) = TM.find tdef t
;;
let v_declared vdec (v : Temp.t) : T.t option = Temp.Map.find vdec v
;;
let v_defined vdef (v : Temp.t) : bool = Temp.Set.mem vdef v
;;

let resolve (omega : T.t TM.t) (tau : T.tau) : T.t =
  match TM.find omega tau with 
  | None -> raise TypeError
  | Some t -> t
;;

let resolve_fsig (fsig : T.fsig) : T.fsig_real option = 
;;
(*_ static_semantic_exp *)

(*_ static_semantic_stm *)

(*_ static_semantic_gdecl *)
let static_semantic_gdecl (gdecl : A.mglob) ({fdef; fdec; tdef; } : global_ctx) : global_ctx =
  match (Mark.data gdecl) with
  | A.Typedef (told, tnew) -> 
    let tnew_sym = (match tnew with 
    | T.RealTyp _ -> raise TypeError
    | T.FakeTyp t -> t) in
    let t = resolve tdef told in
    if SS.mem fdef tnew_sym then raise TypeError
    else if TM.mem tdef tnew then raise TypeError
    else { fdef; fdec; tdef=TM.add_exn tdef ~key:tnew ~data:t; }
  | A.Fundecl (f, fsig) -> (
    match f_declared fdec f with 
    | None -> ()
    | Some fsig' -> ()
  )
  | A.Fundef (f, fsig, ms) -> failwith "not implemented"
;;

let static_semantic ~hdr:A.program ~src:A.program = ()
;; *)