(* open Core
module AS = Assem_l4

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

type jparams = TS.t LM.t (*_ map from bb lab to EOB jump_tag parameters *)

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
let temp_renaming_use (t : Temp.t) (told2new : told2new) : Temp.t =
  match TH.find told2new t with
  | None ->
    (*_ this temp is not recorded; init to a fresh temp *)
    let t' = Temp.create () in
    let () = TH.update told2new t ~f:(fun _ -> t') in
    t'
  | Some t' -> t'
;;

let oper_renaming_use (operand : AS.operand) (told2new : told2new) : AS.operand =
  (*_ produce the operand but with temp replaced; side-effect: update told2new *)
  match operand with
  | AS.Temp t -> AS.Temp (temp_renaming_use t told2new) (*_ if recorded, use the record *)
  | _ -> operand
;;

let oper_renaming_def (operand : AS.operand) (told2new : told2new) : AS.operand =
  match operand with
  | AS.Temp t ->
    let t' = Temp.create () in
    let () = TH.update told2new t ~f:(fun _ -> t') in
    AS.Temp t'
  | _ -> operand
;;

let instr_renaming (instr : AS.instr) (told2new : told2new) : AS.instr =
  (*_ produce the same instr but with temp replaced; side-effect: update told2new *)
  match instr with
  | PureBinop binop ->
    (*_ careful of dest *)
    let lhs = oper_renaming_use binop.lhs told2new in
    let rhs = oper_renaming_use binop.rhs told2new in
    let dest = oper_renaming_def binop.dest told2new in
    PureBinop { binop with lhs; rhs; dest }
  | EfktBinop ebop ->
    (*_ careful of dest *)
    let lhs = oper_renaming_use ebop.lhs told2new in
    let rhs = oper_renaming_use ebop.rhs told2new in
    let dest = oper_renaming_def ebop.dest told2new in
    EfktBinop { ebop with lhs; rhs; dest }
  | Unop uop ->
    (*_ careful of dest *)
    let dest = oper_renaming_def uop.dest told2new in
    Unop { uop with dest }
  | Mov mov ->
    (*_ careful of dest *)
    let src = oper_renaming_use mov.src told2new in
    let dest = oper_renaming_def mov.dest told2new in
    Mov { mov with src; dest }
  | MovSxd { dest; src } ->
    let src = oper_renaming_use src told2new in
    let dest = oper_renaming_def dest told2new in
    MovSxd { dest; src }
  | MovFrom movfrom ->
    let src = oper_renaming_use movfrom.src told2new in
    let dest = oper_renaming_def movfrom.dest told2new in
    MovFrom { movfrom with src; dest }
  | MovTo movto ->
    let src = oper_renaming_use movto.src told2new in
    let dest = oper_renaming_use movto.dest told2new in
    MovTo { movto with src; dest }
  | Set set ->
    let src = oper_renaming_def set.src told2new in
    Set { set with src }
  | Cmp cmp ->
    let lhs = oper_renaming_use cmp.lhs told2new in
    let rhs = oper_renaming_use cmp.rhs told2new in
    Cmp { cmp with lhs; rhs }
  | LoadFromStack lstck ->
    let lstck' = List.map lstck ~f:(fun (t, sz) -> temp_renaming_use t told2new, sz) in
    LoadFromStack lstck'
  | Call call ->
    let args_overflow =
      List.map call.args_overflow ~f:(fun (t, sz) -> temp_renaming_use t told2new, sz)
    in
    Call { call with args_overflow }
  | Jmp _ -> instr
  | Cjmp _ -> instr
  | Ret -> instr
  | Lab _ -> instr
  | AssertFail -> instr
  | _ -> instr
;;

let global_renaming (prog : AS.program) (lparams : lparams) : lparams * AS.program =
  let told2new : told2new = TH.of_alist_exn [] in
  failwith "no"
;; *)
