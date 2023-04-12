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
module IComp = Comparable.Make (Int)
module IS = IComp.Set

(*_ for global renaming *)
type lparams = TS.t LM.t (*_ map from bb lab to SOB parameters *)
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

type told2new = Temp.t TH.t

(*_ for creating phi functions *)

type phi =
  { self : Temp.t (* ; size : AS.size *)
  ; alt_selves : (Label.bt * AS.operand) list
  }

type instr =
  | ASInstr of AS.instr
  | Phi of phi
  | Nop

type block_ssa =
  { (*_ each block has a matrix of (orig)temp -> (pred) lab to *)
    label : Label.bt
  ; code : AS.instr list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  ; bparams : params
  ; jtag : jtag
  }

type fspace_ssa =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; fdef : block_ssa list
  ; cfg_pred : LS.t LM.t
  ; l2jtag : jtag LM.t
  ; tmp_cnt : int
  }

type program_ssa = fspace_ssa list

type block_phi =
  { label : Label.bt
  ; code : instr list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  }

type fspace_phi =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; fdef : block_phi list
  ; tmp_cnt : int
  }

type program_phi = fspace_phi list

(*_ some high level debug function *)
let pp_args (args : (Temp.t * AS.size) list) : string =
  List.map args ~f:(fun (t, sz) -> sprintf "%s:%s" (Temp.name t) (AS.format_size sz))
  |> String.concat ~sep:", "
;;

let pp_params (params : params) : string =
  let stuff = TM.to_alist params in
  List.map stuff ~f:(fun (told, tnew) ->
    sprintf "%s:%s" (Temp.name told) (Temp.name tnew))
  |> String.concat ~sep:", "
;;

let pp_jtag (jtag : jtag) : string =
  match jtag with
  | JRet -> "jret"
  | JUncon { l; params } -> sprintf "goto %s(%s)" (Label.name_bt l) (pp_params params)
  | JCon { lt; tparams; lf; fparams } ->
    sprintf
      "if t:{%s(%s)} f:{%s(%s)}"
      (Label.name_bt lt)
      (pp_params tparams)
      (Label.name_bt lf)
      (pp_params fparams)
;;

let pp_block_ssa ({ label; code; bparams; jtag; _ } : block_ssa) ({ block; _ } : AS.block)
  : string
  =
  sprintf
    "----------------- blc %s(%s) ------------------\n\
     %s\n\
     -------------- jtag:%s -----------------\n"
    (Label.name_bt label)
    (pp_params bparams)
    (List.map (List.zip_exn code block) ~f:(fun (instr, ref_instr) ->
       AS.format_instr ref_instr ^ " ==> " ^ AS.format_instr instr)
    |> String.concat ~sep:"\n")
    (pp_jtag jtag)
;;

let pp_lset (lset : LS.t) : string =
  let stuff = LS.to_list lset in
  List.map stuff ~f:Label.name_bt |> String.concat ~sep:", "
;;

let pp_cfg_pred (cfg : LS.t LM.t) : string =
  let stuff = LM.to_alist cfg in
  sprintf
    "{\n%s\n}"
    (List.map stuff ~f:(fun (l, ls) -> sprintf "%s:%s" (Label.name_bt l) (pp_lset ls))
    |> String.concat ~sep:"\n")
;;

let pp_l2jtag (l2jtag : jtag LM.t) : string =
  let stuff = LM.to_alist l2jtag in
  sprintf
    "{\n%s\n}"
    (List.map stuff ~f:(fun (l, jtag) -> sprintf "%s:%s" (Label.name_bt l) (pp_jtag jtag))
    |> String.concat ~sep:"\n")
;;
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
    let lstck' = List.map lstck ~f:(fun (t, sz) -> temp_rename_def t told2new, sz) in
    LoadFromStack lstck'
  | Call call ->
    let args_overflow =
      List.map call.args_overflow ~f:(fun (t, sz) -> oper_rename_use t told2new, sz)
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
      (* let args' = List.map args ~f:(fun t -> temp_rename_use t told2new) in *)
      FunName { fname; args }
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

let fspace_rename (fspace : AS.fspace) : fspace_ssa =
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
  let lparams : lparams = LM.map block_in ~f:opset2tmpset in
  let told2new : told2new = TH.of_alist_exn [] in
  (*_ 0. forcefully rename function args *)
  (* let args = List.map fspace.args ~f:(fun (t, sz) -> temp_rename_def t told2new, sz) in *)
  let args = fspace.args in
  let hack_func_bt =
    Label.FunName { fname = fspace.fname; args = List.map fspace.args ~f:fst }
  in
  let hack_fun_lset : LS.t = LM.find_exn cfg_pred hack_func_bt in
  let hack_func_bt' =
    Label.FunName { fname = fspace.fname; args = List.map args ~f:fst }
  in
  let cfg_pred =
    LM.remove cfg_pred hack_func_bt |> LM.add_exn ~key:hack_func_bt' ~data:hack_fun_lset
  in
  (*_ 1. iterate over blocks to apply block_rename *)
  let l2blcssa =
    List.map fspace.fdef_blocks ~f:(fun blc -> block_rename blc told2new lparams)
    |> List.map ~f:(fun blcssa -> blcssa.label, (blcssa, blcssa.bparams, blcssa.jtag))
  in
  let fdef_new = List.map l2blcssa ~f:(fun (_, (blc, _, _)) -> blc) in
  let l2jtag =
    List.map l2blcssa ~f:(fun (l, (_, _, jtag)) -> l, jtag) |> LM.of_alist_exn
  in
  { fname = fspace.fname
  ; args
  ; fdef = fdef_new
  ; cfg_pred
  ; l2jtag
  ; tmp_cnt = fspace.tmp_cnt
  }
;;

let global_rename (prog : AS.program) : program_ssa = List.map prog ~f:fspace_rename

let block_phi
    (cfg_pred : LS.t LM.t)
    (l2jtag : jtag LM.t)
    ({ label = lcur; code; jump; depth; bparams; _ } : block_ssa)
    : block_phi
  =
  (*_ block_phi:
  create phi functions: each blcssa has this information that can construct 
  this 2-d matrix below:
                x:x1  y:y2  z:z1
            L1 [x:x2  y:y2  z:z0]
            L2 [x:x1  y:y1  z:z1]
            L3 [x:x2  y:y3  z:z2]
        The columns are phi functions 
               x:x1  y:y2  z:z1
            L1 |x2|  |y2|  |z0|
            L2 |x1|  |y1|  |z1|
            L3 |x2|  |y3|  |z2|
  *)
  (*_ orig temp to fresh ones *)
  let () = printf "lcur is %s\n" (Label.name_bt lcur) in
  let preds = LM.find_exn cfg_pred lcur |> LS.to_list in
  (*_ for each predecessor block, find jtag and gather the their parameter *)
  let preds_jtag =
    List.map preds ~f:(fun lpred ->
        printf "lpred is %s\n" (Label.name_bt lpred);
        lpred, LM.find_exn l2jtag lpred)
  in
  let see_jtag (lpred, jtag) =
    match jtag with
    | JRet -> None
    | JUncon { l; params } ->
      if not (Label.equal_bt lcur l)
      then failwith "ssa: renaming error"
      else Some (lpred, params)
    | JCon { lt; tparams; lf; fparams } ->
      if Label.equal_bt lcur lt
      then Some (lpred, tparams)
      else if Label.equal_bt lcur lf
      then Some (lpred, fparams)
      else failwith "ssa: renaming error"
  in
  let preds_params = List.filter_map preds_jtag ~f:see_jtag in
  let preds_params =
    (*_ tnew -> (tcur * label) *)
    List.map preds_params ~f:(fun (l, params) ->
        List.map (TM.to_alist params) ~f:(fun (told, tnew) ->
            let tcur = TM.find_exn bparams told in
            (*_ replace with latest temp in current params *)
            tcur, (l, tnew)))
    |> List.concat
  in
  let t2nodes = TM.of_alist_multi preds_params |> TM.to_alist in
  let phies : phi list =
    List.map t2nodes ~f:(fun (self, alt) ->
        { self; alt_selves = List.map alt ~f:(fun (l, t) -> l, AS.Temp t) })
  in
  let code : instr list =
    List.map phies ~f:(fun instr -> Phi instr)
    @ List.map code ~f:(fun instr -> ASInstr instr)
  in
  { label = lcur; code; jump; depth }
;;

let fspace_phi ({ fname; args; fdef; cfg_pred; l2jtag; tmp_cnt } : fspace_ssa)
    : fspace_phi
  =
  let fdef = List.map fdef ~f:(block_phi cfg_pred l2jtag) in
  (*_ find the first block after rename and get the temps *)
  let fst_blc = List.hd_exn fdef in
  let fst_blc_params =
    match fst_blc.label with
    | FunName { args; _ } -> args
    | _ ->
      failwith "ssa: renaming error, first block of fspace is not a function name block"
  in
  let sizes = List.map args ~f:snd in
  let args_new = List.zip_exn fst_blc_params sizes in
  (*_ result *)
  { fname; args = args_new; fdef; tmp_cnt }
;;

let global_phi (prog : program_ssa) : program_phi = List.map prog ~f:fspace_phi

(*_ ********* Above: global renaming >>> Below: globaling lining ******* *)
type block =
  { label : Label.bt
  ; lines : int list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  }

type fspace =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; code : instr IH.t
  ; block_info : block list
  ; tuse : IS.t TH.t
  ; tmp_cnt : int
  }

type program = fspace list

let instr_use (instr : instr) : Temp.t list =
  match instr with
  | ASInstr instr ->
    let _, use = SP.def_n_use instr in
    let use = V.vertex_set_to_op_set use in
    let use =
      AS.Set.to_list use
      |> List.filter_map ~f:(fun op ->
             match op with
             | AS.Temp t -> Some t
             | _ -> None)
    in
    use
  | Phi { alt_selves; _ } ->
    let ts =
      List.filter_map alt_selves ~f:(fun (_, op) ->
          match op with
          | AS.Temp t -> Some t
          | _ -> None)
    in
    ts
  | Nop -> []
;;

let blocks_lining (fdef : block_phi list) : instr IH.t * block list =
  (*_ produce code & block_info *)
  let lined_codes : instr IH.t = IH.of_alist_exn [] in
  let to_blocks
      ((blocks, acc) : block list * int)
      ({ label; code; jump; depth } : block_phi)
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
    { label; lines; jump; depth } :: blocks, acc + List.length lines
  in
  let block_info, _ = List.fold_left fdef ~init:([], 0) ~f:to_blocks in
  let block_info = List.rev block_info in
  lined_codes, block_info
;;

let fspace_use (fdef : instr IH.t) : IS.t TH.t =
  let tuse : IS.t TH.t = TH.of_alist_exn [] in
  let iterf ~key:idx ~data:instr =
    let tuses = instr_use instr in
    let update = function
      | None -> IS.singleton idx
      | Some usesite -> IS.add usesite idx
    in
    List.iter tuses ~f:(fun t -> TH.update tuse t ~f:update)
  in
  let () = IH.iteri fdef ~f:iterf in
  tuse
;;

let fspace_lining ({ fname; args; fdef; tmp_cnt } : fspace_phi) : fspace =
  (*_ 0. produce code & block_info : instr IH.t, block LM.t *)
  let code, block_info = blocks_lining fdef in
  let tuse = fspace_use code in
  { fname; args; code; block_info; tuse; tmp_cnt }
;;

let global_lining (prog : program_phi) : program = List.map prog ~f:fspace_lining

(*_ **** ssa function **** *)
let ssa (prog : AS.program) : program =
  let prog_ssa = global_rename prog in
  let prog_phi = global_phi prog_ssa in
  let res = global_lining prog_phi in
  res
;;

(*_ ***** de-ssa function ****** *)
let reconstruct_blocks
    (l2instrs : AS.instr list LT.t)
    (phies : phi list)
    (block_info : block list)
    : AS.block list
  =
  (*_ for each phi t <- {l:op}, and for each l, append Mov {t, op} *)
  let mapf ({ self; alt_selves } : phi) : (Label.bt * AS.instr) list =
    List.map alt_selves ~f:(fun (lpred, op) ->
        lpred, AS.Mov { dest = AS.Temp self; src = op; size = AS.S })
  in
  let extra_moves = List.map phies ~f:mapf |> List.concat |> LM.of_alist_multi in
  (*_ feed extra moves to corresponding labels in l2instrs *)
  let assemble ~key:l ~(data : AS.instr list) : unit =
    let code_rev = LT.find_exn l2instrs l |> List.rev in
    let jmp, rest_rev =
      match code_rev with
      | hd :: tl -> hd, tl
      | _ -> failwith "ssa: de-ssa encounter abnormal block"
    in
    let code = jmp :: (data @ rest_rev) |> List.rev in
    LT.update l2instrs l ~f:(fun _ -> code)
  in
  let () = LM.iteri extra_moves ~f:assemble in
  (*_ construct AS.block using l2instrs and block_info *)
  List.map block_info ~f:(fun { label; jump; depth; _ } : AS.block ->
      { label; block = LT.find_exn l2instrs label; jump; depth })
;;

let reconstruct_fspace ({ fname; args; code; block_info; _ } : fspace) : AS.fspace =
  (*_ for each b in block_info, reconstruct AS.block *)
  let assemble (lines : int list) : AS.instr list * phi list =
    let lines = List.sort ~compare:Int.compare lines in
    (*_ ensure instr is in the right order *)
    let codes = List.map lines ~f:(fun i -> IH.find_exn code i) in
    let fltr_instr = function
      | ASInstr instr -> Some instr
      | _ -> None
    in
    let instr_lst = List.filter_map codes ~f:fltr_instr in
    let fltr_phi = function
      | Phi phi -> Some phi
      | _ -> None
    in
    let phi_lst = List.filter_map codes ~f:fltr_phi in
    instr_lst, phi_lst
  in
  let l2instrs, phies =
    List.map block_info ~f:(fun blc -> blc.label, assemble blc.lines)
    |> List.map ~f:(fun (l, (instrL, phiL)) -> (l, instrL), phiL)
    |> List.unzip
  in
  let l2instrs = LT.of_alist_exn l2instrs in
  let phies = List.concat phies in
  (*_ for each predecessor blocks *)
  let fdef_blocks = reconstruct_blocks l2instrs phies block_info in
  (* tmp count is undermined as we are already in ssa, so we do not care about temp count *)
  { fname; args; fdef_blocks; tmp_cnt = -1 }
;;

let de_ssa (prog : program) : AS.program = List.map prog ~f:reconstruct_fspace

(*_ debug functions *)

let pp_fspace_ssa
    ({ fname; args; fdef; cfg_pred; l2jtag; _ } : fspace_ssa)
    ({ fdef_blocks; _ } : AS.fspace)
    : string
  =
  sprintf
    "==================================\n\
     fspace_ssa %s(%s):\n\
     %s\n\
     cfg=%s\n\
     l2jtag=%s\n\
     ===================================\n"
    (Symbol.name fname)
    (pp_args args)
    (List.map (List.zip_exn fdef fdef_blocks) ~f:(fun (b, bref) -> pp_block_ssa b bref)
    |> String.concat ~sep:"\n")
    (pp_cfg_pred cfg_pred)
    (pp_l2jtag l2jtag)
;;

let pp_program_ssa (prog : program_ssa) (ref : AS.program) : string =
  List.map (List.zip_exn prog ref) ~f:(fun (p, ref) -> pp_fspace_ssa p ref)
  |> String.concat ~sep:"\n\n"
;;

let pp_phi ({ self; alt_selves } : phi) : string =
  sprintf
    "%s <-- @(%s)"
    (Temp.name self)
    (List.map alt_selves ~f:(fun (l, op) ->
         sprintf "%s:%s" (Label.name_bt l) (AS.format_operand op))
    |> String.concat ~sep:", ")
;;

let pp_block_phi
    (({ label; code; _ }, { code = code'; bparams; jtag; _ }) : block_phi * block_ssa)
    : string
  =
  let isphi = function
    | Phi phi -> Some (pp_phi phi)
    | _ -> None
  in
  let phies = List.filter_map code ~f:isphi in
  let asinstr =
    List.filter_map code ~f:(fun instr ->
        match instr with
        | ASInstr i -> Some i
        | _ -> None)
  in
  sprintf
    "----------------- blc %s (%s) ------------------\n\
     %s\n\
     %s\n\
     -------------- jump:%s -----------------\n"
    (Label.name_bt label)
    (pp_params bparams)
    (String.concat phies ~sep:"\n")
    (List.map (List.zip_exn asinstr code') ~f:(fun (i1, i2) ->
         sprintf "%s >>> %s" (AS.format_instr i1) (AS.format_instr i2))
    |> String.concat ~sep:"\n")
    (pp_jtag jtag)
;;

let pp_fspace_phi
    (({ fname; args; fdef; _ }, { fdef = fdef_ssa; _ }) : fspace_phi * fspace_ssa)
    : string
  =
  sprintf
    "==================================\n\
     fspace_phi %s(%s):\n\
     %s\n\
     ===================================\n"
    (Symbol.name fname)
    (pp_args args)
    (List.map (List.zip_exn fdef fdef_ssa) ~f:pp_block_phi |> String.concat ~sep:"\n")
;;

let pp_program_phi (prog : program_phi) (ref : program_ssa) : string =
  List.map (List.zip_exn prog ref) ~f:pp_fspace_phi |> String.concat ~sep:"\n\n"
;;

let pp_instr (l : int) (instr : instr) : string =
  match instr with
  | ASInstr instr -> Int.to_string l ^ " : " ^ AS.format_instr instr
  | Phi phi -> Int.to_string l ^ " : " ^ pp_phi phi
  | Nop -> Int.to_string l ^ " : nop"
;;

let pp_block ({ label; lines; jump; _ } : block) (code : instr IH.t) : string =
  let l2code =
    List.map lines ~f:(fun l ->
        let instr = IH.find_exn code l in
        pp_instr l instr)
  in
  sprintf
    "----------------- blc %s. ------------------\n\
     %s\n\
     -------------- jump:%s -----------------\n"
    (Label.name_bt label)
    (String.concat l2code ~sep:"\n")
    (AS.format_jump_tag jump)
;;

let pp_tuse (tuse : IS.t TH.t) : string =
  let stuff = TH.to_alist tuse in
  let stuff = List.map stuff ~f:(fun (t, lset) -> t, IS.to_list lset) in
  List.map stuff ~f:(fun (t, ilst) ->
      sprintf
        "%s:%s"
        (Temp.name t)
        (List.map ilst ~f:Int.to_string |> String.concat ~sep:", "))
  |> String.concat ~sep:"\n"
;;

let pp_fspace ({ fname; args; code; block_info; tuse; _ } : fspace) : string =
  sprintf
    "===================================\n\
     fspace %s(%s):\n\
     %s\n\
     -----------------------------------\n\
     tuse = {\n\
     %s\n\
     }\n\n\
    \     ===================================\n"
    (Symbol.name fname)
    (pp_args args)
    (List.map block_info ~f:(fun b -> pp_block b code) |> String.concat ~sep:"\n")
    (pp_tuse tuse)
;;

let pp_program (prog : program) : string =
  List.map prog ~f:pp_fspace |> String.concat ~sep:"\n\n"
;;

(* let ssa_debug (prog : AS.program) : unit =
  (* let () = printf "dumping 3-assem...\n%s\n\n" (AS.format_program prog) in *)
  let prog_ssa : program_ssa = global_rename prog in
  (* let () = `printf "dumping prog after ssa...\n\n%s\n\n" (pp_program_ssa prog_ssa prog) in *)
  let prog_phi : program_phi = global_phi prog_ssa in
  (* let () =
    printf
      "dumping prog after phi transformation...\n\n%s\n\n"
      (pp_program_phi prog_phi prog_ssa)
  in *)
  let prog : program = global_lining prog_phi in
  let () = printf "dumping prog after lining...\n\n%s\n\n" (pp_program prog) in
  let prog : program = Propagation.propagate prog in
  let prog : AS.program = de_ssa prog in
  let () = printf "dumping AS.program after de_ssa ... \n\n%s\n\n" (AS.format_program prog) in
  ()
;; *)
