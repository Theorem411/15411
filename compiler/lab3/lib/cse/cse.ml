open Core
module SSA = Ssa
module AS = Assem_l4
module TH = SSA.TH
module LS = Cfg.LS
module LM = Cfg.LM
module IH = SSA.IH
module IM = SSA.IM
module IS = SSA.IS

(*_ bank functionalities *)
module CSEKey = struct
  type t =
    | PureBinop of
        { lhs : AS.operand
        ; size : AS.size
        ; op : AS.pure_operation
        ; rhs : AS.operand
        }
    | LeaPointer of
        { size : AS.size
        ; base : AS.operand
        ; offset : int
        }
    | LeaArray of
        { base : AS.operand
        ; offset : int
        ; index : AS.operand
        ; scale : int (* can be only 1,2,4,8 *)
        }
  [@@deriving hash, compare, equal, sexp]
end

module CSEH = Hashtbl.Make (CSEKey)


type cse_t =
  { dest : AS.operand
  ; in_blc : Label.bt
  }

type bank = cse_t CSEH.t

let cse_line
  (bk : bank)
  (dt : Dt.t)
  (code : SSA.instr IH.t)
  (tuse : IS.t TH.t)
  (ln2blc : Label.bt IM.t)
  (ln : int)
  : unit
  =
  (*_ might modify code & tuse; only insert into bk and no overwritten *)
  let instr = IH.find_exn code ln in
  let in_blc = IM.find_exn ln2blc ln in
  match instr with
  | ASInstr (AS.PureBinop { dest; size; lhs; op; rhs }) ->
    let cse = CSEKey.PureBinop { lhs; size; op; rhs } in
    (match CSEH.find bk cse with
     | None ->
       (*_ insert (cse, {dest, blc.label}) into bank *)
       CSEH.update bk cse ~f:(fun _ -> { dest; in_blc })
     | Some { dest = dest'; in_blc = in_blc' } ->
       (*_ if dominate(in_blc, l) then update code[ln] := dest <- dest' *)
       (match Dt.is_dominate dt in_blc' in_blc with
        | Same | IDom ->
          (*_ update code *)
          let new_instr = SSA.ASInstr (AS.Mov { dest; src = dest'; size }) in
          let () = IH.update code ln ~f:(fun _ -> new_instr) in
          (*_ update tuse: delete ln from tuse[lhs] and tuse[rhs]; add ln to tuse[dest'] *)
          let () =
            match lhs with
            | AS.Temp t -> SSA.tuse_remove_ln tuse t ln
            | _ -> ()
          in
          let () =
            match rhs with
            | AS.Temp t -> SSA.tuse_remove_ln tuse t ln
            | _ -> ()
          in
          let () =
            match dest' with
            | AS.Temp t -> SSA.tuse_add_ln tuse t ln
            | _ -> ()
          in
          ()
        | Nope -> ()))
  | ASInstr (AS.LeaPointer { dest; size; base; offset }) ->
    let cse = CSEKey.LeaPointer { base; size; offset } in
    (match CSEH.find bk cse with
     | None -> CSEH.update bk cse ~f:(fun _ -> { dest; in_blc })
     | Some { dest = dest'; in_blc = in_blc' } ->
       (match Dt.is_dominate dt in_blc' in_blc with
        | Same | IDom ->
          (*_ update code *)
          let new_instr = SSA.ASInstr (AS.Mov { dest; src = dest'; size }) in
          let () = IH.update code ln ~f:(fun _ -> new_instr) in
          (*_ update tuse: delete ln from tuse[lhs] and tuse[rhs]; add ln to tuse[dest'] *)
          let () =
            match base with
            | AS.Temp t -> SSA.tuse_remove_ln tuse t ln
            | _ -> ()
          in
          let () =
            match dest' with
            | AS.Temp t -> SSA.tuse_add_ln tuse t ln
            | _ -> ()
          in
          ()
        | Nope -> ()))
  | ASInstr (AS.LeaArray { dest; base; offset; index; scale }) ->
    let cse = CSEKey.LeaArray { base; offset; index; scale } in
    (match CSEH.find bk cse with
     | None -> CSEH.update bk cse ~f:(fun _ -> { dest; in_blc })
     | Some { dest = dest'; in_blc = in_blc' } ->
       (match Dt.is_dominate dt in_blc' in_blc with
        | Same | IDom ->
          (*_ update code *)
          let new_instr = SSA.ASInstr (AS.Mov { dest; src = dest'; size = AS.L }) in
          let () = IH.update code ln ~f:(fun _ -> new_instr) in
          (*_ update tuse: delete ln from tuse[lhs] and tuse[rhs]; add ln to tuse[dest'] *)
          let () =
            match base with
            | AS.Temp t -> SSA.tuse_remove_ln tuse t ln
            | _ -> ()
          in
          let () =
            match index with
            | AS.Temp t -> SSA.tuse_remove_ln tuse t ln
            | _ -> ()
          in
          let () =
            match dest' with
            | AS.Temp t -> SSA.tuse_add_ln tuse t ln
            | _ -> ()
          in
          ()
        | Nope -> ()))
  | _ -> ()
;;

let cse_block
  (bk : bank)
  (dt : Dt.t)
  (code : SSA.instr IH.t)
  (tuse : IS.t TH.t)
  (ln2blc : Label.bt IM.t)
  (block : SSA.block)
  : unit
  =
  List.iter block.lines ~f:(fun i -> cse_line bk dt code tuse ln2blc i)
;;

(*_ export functionalities *)
let cleanup (fspace : SSA.fspace) (dead : Label.bt list) : SSA.fspace =
  let ({ code; block_info; tuse; _ } : SSA.fspace) = fspace in
  let dead_blcs = LS.of_list dead in
  let b2codes =
    List.map block_info ~f:(fun blc -> blc.label, blc.lines) |> LM.of_alist_exn
  in
  let dead_lns =
    List.map dead ~f:(fun l -> LM.find_exn b2codes l) |> List.concat |> IS.of_list
  in
  let block_info' =
    List.filter block_info ~f:(fun blc -> not (LS.mem dead_blcs blc.label))
  in
  let () = IS.iter dead_lns ~f:(fun dln -> IH.remove code dln) in
  let () =
    TH.iteri tuse ~f:(fun ~key:t ~data:lns ->
      TH.update tuse t ~f:(fun _ -> IS.diff lns dead_lns))
  in
  { fspace with code; block_info = block_info'; tuse }
;;

let cse_fspace (fspace : SSA.fspace) : SSA.fspace =
  (*_ to cfg *)
  let cfg_input = SSA.to_cfg fspace in
  let ({ dead; _ } : Cfg.cfg_output) = Cfg.create cfg_input in
  (*_ some preprocess *)
  let fspace = cleanup fspace dead in
  let cfg_input = SSA.to_cfg fspace in
  let ({ cfg; dead } : Cfg.cfg_output) = Cfg.create cfg_input in
  let () = if List.length dead <> 0 then failwith "wtf" else () in
  (*_ *)
  let ({ code; tuse; block_info; _ } : SSA.fspace) = fspace in
  let dt = Dt.dominator_tree cfg in
  let blclist = Dt.dominator_tree_post_order dt in
  let bk = CSEH.of_alist_exn [] in
  let ln2blc =
    List.map block_info ~f:(fun { label; lines; _ } ->
      List.map lines ~f:(fun ln -> ln, label))
    |> List.concat
    |> IM.of_alist_exn
  in
  let lab2blc = List.map block_info ~f:(fun blc -> blc.label, blc) |> LM.of_alist_exn in
  let () =
    List.iter blclist ~f:(fun l ->
      cse_block bk dt code tuse ln2blc (LM.find_exn lab2blc l))
  in
  fspace
;;

let cse (prog : SSA.program) : SSA.program = List.map prog ~f:cse_fspace
