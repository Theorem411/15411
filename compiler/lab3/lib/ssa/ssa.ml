(* open Core
module AS = Assem_l4
module Live = Live_faster
module L = struct
  type t = Label.bt [@@deriving equal, compare, sexp]
end

module LM = Map.Make (L)
module LS = Set.Make (L)

module T = struct
  type t = Temp.t [@@deriving equal, compare, sexp, hash]
end

module TM = Map.Make (T)
module TS = Set.Make (T)
module TH = Hashtbl.Make (T)

type lparams = TS.t LM.t (*_ map from bb lab to SOB parameters *)
type lpreds = LS.t LM.t (*_ map from bb lab to pred bbs' labs *)

type jtag =
  | JRet
  | JUncon of
      { l : Label.bt
      ; params : TS.t
      }
  | JCon of
      { lt : Label.bt
      ; tparams : TS.t
      ; lf : Label.bt
      ; fparams : TS.t
      }

type jparams = jtag LM.t (*_ map from bb lab to EOB jump_tag parameters *)

type phi_t =
  { self : Temp.t
  ; alt_self : (Temp.t * Label.bt) list
  }

type lphi = phi_t list LM.t (*_ map from bb lab to all the phi functions *)

type use_site_t =
  | LN of Label.bt * int
  | PhiId of Label.bt

type tuse_site = use_site_t TM.t
type told2new = Temp.t TH.t

(*_ ********************* Above: type names for sanity ********************** *)
let temp_rename_use (t : Temp.t) (told2new : told2new) : Temp.t =
  (*_ rename temps if used: 
     - if already renamed and stored in told2new, use that version 
     - if haven't encountered before, replace by new version and stored in told2new *)
  match TH.find told2new t with
  | None ->
    (*_ this temp is not recorded; init to a fresh temp *)
    let t' = Temp.create () in
    let () = TH.update told2new t ~f:(fun _ -> t') in
    t'
  | Some t' -> t'
;;

let temp_rename_def (t : Temp.t) (told2new : told2new) : unit =
  let t' = Temp.create () in
  TH.update told2new t ~f:(fun _ -> t')
;;

let oper_rename_use (operand : AS.operand) (told2new : told2new) : AS.operand =
  (*_ produce the operand but with temp replaced; side-effect: update told2new *)
  match operand with
  | AS.Temp t -> AS.Temp (temp_rename_use t told2new) (*_ if recorded, use the record *)
  | _ -> operand
;;

let oper_rename_def (operand : AS.operand) (told2new : told2new) : AS.operand =
  match operand with
  | AS.Temp t ->
    let t' = Temp.create () in
    let () = TH.update told2new t ~f:(fun _ -> t') in
    AS.Temp t'
  | _ -> operand
;;

let instr_rename (instr : AS.instr) (told2new : told2new) : AS.instr =
  (*_ produce the same instr but with temp replaced; side-effect: update told2new *)
  match instr with
  | PureBinop binop ->
    (*_ careful of dest *)
    let lhs = oper_rename_use binop.lhs told2new in
    let rhs = oper_rename_use binop.rhs told2new in
    let dest = oper_rename_def binop.dest told2new in
    PureBinop { binop with lhs; rhs; dest }
  | EfktBinop ebop ->
    (*_ careful of dest *)
    let lhs = oper_rename_use ebop.lhs told2new in
    let rhs = oper_rename_use ebop.rhs told2new in
    let dest = oper_rename_def ebop.dest told2new in
    EfktBinop { ebop with lhs; rhs; dest }
  | Unop uop ->
    (*_ careful of dest *)
    let dest = oper_rename_def uop.dest told2new in
    Unop { uop with dest }
  | Mov mov ->
    (*_ careful of dest *)
    let src = oper_rename_use mov.src told2new in
    let dest = oper_rename_def mov.dest told2new in
    Mov { mov with src; dest }
  | MovSxd { dest; src } ->
    let src = oper_rename_use src told2new in
    let dest = oper_rename_def dest told2new in
    MovSxd { dest; src }
  | MovFrom movfrom ->
    let src = oper_rename_use movfrom.src told2new in
    let dest = oper_rename_def movfrom.dest told2new in
    MovFrom { movfrom with src; dest }
  | MovTo movto ->
    let src = oper_rename_use movto.src told2new in
    let dest = oper_rename_use movto.dest told2new in
    MovTo { movto with src; dest }
  | Set set ->
    let src = oper_rename_def set.src told2new in
    Set { set with src }
  | Cmp cmp ->
    let lhs = oper_rename_use cmp.lhs told2new in
    let rhs = oper_rename_use cmp.rhs told2new in
    Cmp { cmp with lhs; rhs }
  | LoadFromStack lstck ->
    let lstck' = List.map lstck ~f:(fun (t, sz) -> temp_rename_use t told2new, sz) in
    LoadFromStack lstck'
  | Call call ->
    let args_overflow =
      List.map call.args_overflow ~f:(fun (t, sz) -> temp_rename_use t told2new, sz)
    in
    Call { call with args_overflow }
  | Jmp _ -> instr
  | Cjmp _ -> instr
  | Ret -> instr
  | Lab _ -> instr
  | AssertFail -> instr
  | _ -> instr
;;

let block_label_rename (params : TS.t) (told2new : told2new) : unit =
  TS.iter params ~f:(fun t -> temp_rename_def t told2new)
;;

let block_renaming (block : AS.block) (told2new : told2new) (lparams : lparams)
  : AS.block * jtag
  =
  (*_ 0. update told2new after seeing label parameters *)
  let params = LM.find_exn lparams block.label in
  let () = block_label_rename params told2new in
  (*_ 1. rename block.block top-to-down *)
  let foldf acc instr =
    let instr' = instr_rename instr told2new in
    instr' :: acc
  in
  let code = List.fold_left block.block ~init:[] ~f:foldf in
  let code = List.rev code in
  (*_ 2. create jump labels *)
  let jtag =
    match block.jump with
    | AS.JRet -> JRet
    | AS.JCon { jt; jf } ->
      let tsucc_params = LM.find_exn lparams (Label.bt jt) in
      let fsucc_params = LM.find_exn lparams (Label.bt jf) in
      let tsucc_params' = TS.map tsucc_params ~f:(fun t -> temp_rename_use t told2new) in
      let fsucc_params' = TS.map fsucc_params ~f:(fun t -> temp_rename_use t told2new) in
      let lt, lf = Label.bt jt, Label.bt jf in
      JCon { lt; lf; tparams = tsucc_params'; fparams = fsucc_params' }
    | AS.JUncon l ->
      let succ_params = LM.find_exn lparams (Label.bt l) in
      let succ_params' = TS.map succ_params ~f:(fun t -> temp_rename_use t told2new) in
      JUncon { l = Label.bt l; params = succ_params' }
  in
  { block with block=code }, jtag
;;

let fspace_renaming (fspace : AS.fspace) : AS.fspace = 
  (*_ 0. do liveness once and get IN[block] *)
  let { block_in_out; _ } : Live.live_package_t = Live.mk_liveness_fspace (Block.of_fspace fspace) in
  let lparams = LM.mapi block_in_out in
  failwith "no"

let global_renaming (prog : AS.program) : lparams * AS.program =
  let told2new : told2new = TH.of_alist_exn [] in
  (*_ call mk_liveness to get block parameters *)
  failwith "no"
;; *)

(*_ ************************ Above: global_renaming ************************ *)
