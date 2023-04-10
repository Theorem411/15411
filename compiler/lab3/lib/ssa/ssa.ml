open Core
module AS = Assem_l4
module B = Block
module Live = Live_faster
module V = Graph.Vertex
module SP = Singlepass
module LM = Live.LM
module LS = Live.LS
module LT = Live.LT

module T = struct
  type t = Temp.t [@@deriving equal, compare, sexp, hash]
end

module TComp = Comparable.Make (T)
module TM = TComp.Map
module TS = TComp.Set
module TH = Hashtbl.Make (T)
module IH = Hashtbl.Make (Int)

(*_ for global renaming *)
type lparams = TS.t LM.t (*_ map from bb lab to SOB parameters *)
type lpreds = LS.t LM.t (*_ map from bb lab to pred bbs' labs *)
type params = Temp.t TM.t

type jtag =
  | JRet
  | JUncon of
      { l : Label.bt
      ; params : params
      }
  | JCon of
      { lt : Label.bt
      ; tparams : params
      ; lf : Label.bt
      ; fparams : params
      }

type jparams = jtag LM.t (*_ map from bb lab to EOB jump_tag parameters *)
type told2new = Temp.t TH.t

(*_ for creating phi functions *)

type node =
  { temp : Temp.t
  ; pred : Label.bt
  }

type phi = AS.operand LM.t TM.t
(*_ t <- \phi(l:t') *)

type block_ssa =
  { (*_ each block has a matrix of (orig)temp -> (pred) lab to *)
    label : Label.bt
  ; code : AS.instr list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  ; bparams : params
  ; jtag : jtag
  }

type block_phi =
  { label : Label.bt
  ; code : AS.instr list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  ; phies : phi
  }

type fspace_phi =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; fdef : block_phi list
  }

type program_phi = fspace_phi list

(*_ program ssa : make public *)

(*_ for phi-optimization and more *)
type phi_use_site_t =
  { which_blc : Label.bt
  ; which_temp : Temp.t
  ; which_pred : Label.bt
  }

type use_site_t =
  | LN of int
  | PhiId of phi_use_site_t

type tuse_sites = use_site_t list

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

let temp_rename_def (t : Temp.t) (told2new : told2new) : Temp.t =
  let t' = Temp.create () in
  let () = TH.update told2new t ~f:(fun _ -> t') in
  t'
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

let block_param_rename_def (params : TS.t) (told2new : told2new) : params =
  let ts = TS.to_list params in
  let res = List.map ts ~f:(fun t -> t, temp_rename_def t told2new) in
  TM.of_alist_exn res
;;

let block_param_rename_use (params : TS.t) (told2new : told2new) : params =
  let ts = TS.to_list params in
  let res = List.map ts ~f:(fun t -> t, temp_rename_use t told2new) in
  TM.of_alist_exn res
;;

let block_rename (block : AS.block) (told2new : told2new) (lparams : lparams) : block_ssa =
  (*_ 0. update told2new after seeing label parameters *)
  let livein = LM.find_exn lparams block.label in
  let params = block_param_rename_def livein told2new in
  (*_ 1. renames temps in function label if it's one; use temp_rename_use *)
  let new_label : Label.bt =
    match block.label with
    | FunName { fname; args } ->
      let args' = List.map args ~f:(fun t -> temp_rename_use t told2new) in
      FunName { fname; args = args' }
    | _ -> block.label
  in
  (*_ 2. rename block.block top-to-down *)
  let foldf acc instr =
    let instr' = instr_rename instr told2new in
    instr' :: acc
  in
  let code = List.fold_left block.block ~init:[] ~f:foldf in
  let code = List.rev code in
  (*_ 3. create jump labels *)
  let jtag =
    match block.jump with
    | AS.JRet -> JRet
    | AS.JCon { jt; jf } ->
      let tsucc_params = LM.find_exn lparams (Label.bt jt) in
      let tparams = block_param_rename_use tsucc_params told2new in
      let fsucc_params = LM.find_exn lparams (Label.bt jf) in
      let fparams = block_param_rename_use fsucc_params told2new in
      let lt, lf = Label.bt jt, Label.bt jf in
      JCon { lt; lf; tparams; fparams }
    | AS.JUncon l ->
      let succ_params = LM.find_exn lparams (Label.bt l) in
      let params' = block_param_rename_use succ_params told2new in
      JUncon { l = Label.bt l; params = params' }
  in
  (*_ result *)
  { label = new_label
  ; code
  ; jump = block.jump
  ; depth = block.depth
  ; bparams = params
  ; jtag
  }
;;

let fspace_rename (fspace : AS.fspace) : fspace_phi =
  (*_ 0. do liveness once and get IN[block] *)
  let ({ block_in; cfg_pred; _ } : Live.live_package_t) =
    Live.mk_liveness_fspace (Block.of_fspace fspace)
  in
  let op2temp = function
    | AS.Temp t -> Some t
    | _ -> None
  in
  let opset2tmpset (opset : AS.Set.t) =
    let opset' = AS.Set.to_list opset in
    let res = List.filter_map opset' ~f:op2temp in
    TS.of_list res
  in
  let lparams : lparams = Live.LM.map block_in ~f:opset2tmpset in
  (*_ 1. iterate over blocks to apply block_rename *)
  let told2new : told2new = TH.of_alist_exn [] in
  let l2blcssa =
    List.map fspace.fdef_blocks ~f:(fun blc -> block_rename blc told2new lparams)
    |> List.map ~f:(fun blcssa -> blcssa.label, (blcssa, blcssa.bparams, blcssa.jtag))
  in
  let fdef_new = List.map l2blcssa ~f:(fun (_, (blc, _, _)) -> blc) in
  let l2jtag =
    List.map l2blcssa ~f:(fun (l, (_, _, jtag)) -> l, jtag) |> LM.of_alist_exn
  in
  (*_ 2. create phi functions: *)
  let mapf ({ label = l; code; jump; depth; bparams; _ } : block_ssa) : block_phi =
    (*_ orig temp to fresh ones *)
    let preds = LM.find_exn cfg_pred l |> LS.to_list in
    (*_ for each predecessor block, find jtag and gather the their parameter *)
    let preds_jtag = List.map preds ~f:(fun l -> LM.find_exn l2jtag l) in
    let see_jtag = function
      | JRet -> None
      | JUncon { l = l'; params } ->
        if not (Label.equal_bt l l') then failwith "ssa: renaming error" else Some params
      | JCon { lt; tparams; lf; fparams } ->
        if Label.equal_bt l lt
        then Some tparams
        else if Label.equal_bt l lf
        then Some fparams
        else failwith "ssa: renaming error"
    in
    let preds_params = List.filter_map preds_jtag ~f:see_jtag in
    let preds_params = List.zip_exn preds preds_params in
    let preds_params =
      (*_ (tcur * label) -> tnew *)
      List.map preds_params ~f:(fun (l, params) ->
        List.map (TM.to_alist params) ~f:(fun (told, tnew) ->
          let tcur = TM.find_exn bparams told in
          (*_ replace with latest temp in current params *)
          ({ temp = tcur; pred = l } : node), tnew))
      |> List.concat
    in
    let phies =
      (*_ tcur -> (label * tnew) *)
      List.map preds_params ~f:(fun ({ temp = tcur; pred }, tnew) -> tcur, (pred, tnew))
    in
    let phies : phi =
      TM.of_alist_multi phies
      |> TM.map ~f:(fun lst ->
           LM.of_alist_exn (List.map lst ~f:(fun (l, t) -> l, AS.Temp t)))
    in
    { label = l; code; jump; depth; phies }
  in
  let fdef = List.map fdef_new ~f:mapf in
  (*_ 3. find the first block after rename and get the temps *)
  let fst_blc = List.hd_exn fdef_new in
  let fst_blc_params =
    match fst_blc.label with
    | FunName { args; _ } -> args
    | _ ->
      failwith "ssa: renaming error, first block of fspace is not a function name block"
  in
  let sizes = List.map fspace.args ~f:snd in
  let args_new = List.zip_exn fst_blc_params sizes in
  (*_ result *)
  { fname = fspace.fname; args = args_new; fdef }
;;

let global_renaming (prog : AS.program) : program_phi = List.map prog ~f:fspace_rename

(*_ ********* Above: global renaming >>> Below: globaling lining ******* *)
type block =
  { label : Label.bt
  ; lines : int list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  }

type fspace_lines =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; code : AS.instr IH.t
  ; block_info : block LM.t
  ; phies : phi LT.t
  }

type program_lines = fspace_lines list

let fspace_lining ({ fname; args; fdef } : fspace_phi) : fspace_lines =
  let lined_codes = IH.of_alist_exn [] in
  let glob_phies = LT.of_alist_exn [] in
  let to_blocks
    ((blocks, acc) : block list * int)
    ({ label; code; jump; depth; phies } : block_phi)
    : block list * int
    =
    (*_ annote this blocks' codes with line numbers *)
    let code_line = List.mapi code ~f:(fun idx instr -> idx + acc, instr) in
    let lines = List.map code_line ~f:fst in
    (*_ update global line numbering *)
    let () =
      List.iter code_line ~f:(fun (i, instr) ->
        IH.update lined_codes i ~f:(fun _ -> instr))
    in
    let () = LT.update glob_phies label ~f:(fun _ -> phies) in
    { label; lines; jump; depth } :: blocks, acc + List.length lines
  in
  let block_info, _ = List.fold_left fdef ~init:([], 0) ~f:to_blocks in
  let block_info =
    List.map block_info ~f:(fun blc -> blc.label, blc) |> LM.of_alist_exn
  in
  { fname; args; code = lined_codes; block_info; phies = glob_phies }
;;

let global_lining (prog : program_phi) : program_lines = List.map prog ~f:fspace_lining

(*_ ********* Above: global_lining >>> Below: build temp to use map ******* *)

type fspace =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; code : AS.instr IH.t
  ; block_info : block LM.t
  ; phies : phi LT.t
  ; t2use : tuse_sites TH.t
  }

type program = fspace list

let instr_use (instr : AS.instr) : Temp.t list =
  let use, _ = SP.def_n_use instr in
  let use = V.vertex_set_to_op_set use in
  let use =
    AS.Set.to_list use
    |> List.filter_map ~f:(fun op ->
         match op with
         | AS.Temp t -> Some t
         | _ -> None)
  in
  use
;;

let phi_use (which_blc : Label.bt) (phi : phi) : (AS.operand * phi_use_site_t) list =
  let phi = TM.to_alist phi in
  let phi =
    List.map phi ~f:(fun (which_temp, phi) ->
      LM.to_alist phi
      |> List.map ~f:(fun (which_pred, t) -> t, { which_blc; which_temp; which_pred }))
    |> List.concat
  in
  phi
;;

let fspace_use ({ fname; args; code; block_info; phies } : fspace_lines) : fspace =
  let t2use : tuse_sites TH.t = TH.of_alist_exn [] in
  (*_ go over each line of fspace.code to find temp uses *)
  let iterf ~key:idx ~data:instr =
    let tuses = instr_use instr in
    let update = function
      | None -> [ LN idx ]
      | Some usesite -> LN idx :: usesite
    in
    List.iter tuses ~f:(fun t -> TH.update t2use t ~f:update)
  in
  let () = IH.iteri code ~f:iterf in
  (*_ go over each phi function *)
  let iterf ~key:lab ~(data : phi) =
    let t2phi = phi_use lab data in
    let update phi_use = function
      | None -> [ PhiId phi_use ]
      | Some usesite -> PhiId phi_use :: usesite
    in
    let temp_exn = function
      | AS.Temp t -> t
      | _ -> failwith "ssa: phi should only have temp at this point"
    in
    List.iter t2phi ~f:(fun (t, phi_use) ->
      TH.update t2use (temp_exn t) ~f:(update phi_use))
  in
  let () = LT.iteri phies ~f:iterf in
  { fname; args; code; block_info; phies; t2use }
;;

let global_use (prog : program_lines) : program = List.map ~f:fspace_use prog

(*_ **** ssa function **** *)
let ssa (prog : AS.program) : program =
  global_renaming prog |> global_lining |> global_use
;;

(*_ ***** de-ssa function ****** *)
let fspace_back ({ fname; args; code; block_info; phies; _ } : fspace) : AS.fspace =
  (*_ for each b in block_info, reconstruct AS.block *)
  let assemble (lines : int list) : AS.instr list =
    let lines = List.sort ~compare:Int.compare lines in
    (*_ ensure instr is in the right order *)
    List.map lines ~f:(fun i -> IH.find_exn code i)
  in
  let fdef_blocks = LM.map block_info ~f:(fun blc -> assemble blc.lines) in
  (*_ for each phi function, append moves to the predecessors *)
  let mapf ((_, phi) : Label.bt * phi) =
    let movs = TM.to_alist phi in
    let movs =
      List.map movs ~f:(fun (t, l2o) ->
        let l2o' = LM.to_alist l2o in
        List.map l2o' ~f:(fun (l, o) ->
          l, AS.Mov { dest = AS.Temp t; size = failwith "no"; src = o }))
      |> List.concat
    in
    movs
  in
  let extra_moves = List.map ~f:mapf (LT.to_alist phies) |> List.concat in
  let extra_moves = LM.of_alist_multi extra_moves in
  let fdef_blocks =
    LM.mapi fdef_blocks ~f:(fun ~key:lab ~data:movs ->
      let extra = LM.find_exn extra_moves lab in
      movs @ extra)
  in
  (*_ convert fdef_blocks into the correct type *)
  let fdef_blocks = LM.to_alist fdef_blocks in
  let to_block label block : AS.block =
    let ({ jump; depth; _ } : block) = LM.find_exn block_info label in
    { label; block; jump; depth }
  in
  let fdef_blocks = List.map fdef_blocks ~f:(fun (l, code) -> to_block l code) in
  { fname; args; fdef_blocks }
;;

let de_ssa (prog : program) : AS.program = List.map prog ~f:fspace_back
