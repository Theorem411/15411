open Core
module AS = Assem_l4
module SSA = Ssa
module IH = SSA.IH
module TH = SSA.TH
module IS = SSA.IS

module Instr = struct
  type t = AS.instr [@@deriving compare, equal, sexp, hash]
end

module Operand = struct
  type t = AS.operand [@@deriving compare, equal, sexp, hash]
end

module InstrTbl = Hashtbl.Make (Instr)
module OperandTbl = Hashtbl.Make (Operand)

(* let debug_mode = true
let debug_print (err_msg : string) : unit = if debug_mode then printf "%s" err_msg else ()
let pp_IS (lset : IS.t) : string =
  let lset = IS.to_list lset in
  List.map lset ~f:Int.to_string |> String.concat ~sep:", "
;; *)

module T = struct
  type t = Temp.t [@@deriving equal, compare, sexp, hash]
end

(*_ ************  helper function **************** *)
let op_prop (op : AS.operand) (t : Temp.t) (sub : AS.operand) : AS.operand =
  match op with
  | AS.Temp t' -> if Temp.equal t t' then sub else op
  | _ -> op
;;

let asinstr_prop
    (op_prop : AS.operand -> Temp.t -> AS.operand -> AS.operand)
    (instr : AS.instr)
    (t : Temp.t)
    (sub : AS.operand)
    : AS.instr
  =
  match instr with
  | PureBinop { op; size; dest; lhs; rhs } ->
    let lhs' = op_prop lhs t sub in
    let rhs' = op_prop rhs t sub in
    PureBinop { op; size; dest; lhs = lhs'; rhs = rhs' }
  | EfktBinop { op; dest; lhs; rhs } ->
    let lhs' = op_prop lhs t sub in
    let rhs' = op_prop rhs t sub in
    EfktBinop { op; dest; lhs = lhs'; rhs = rhs' }
  | Unop { op; dest; src } ->
    let src' = op_prop src t sub in
    Unop { op; dest; src = src' }
  | Mov { dest; size; src } ->
    let src' = op_prop src t sub in
    Mov { dest; size; src = src' }
  | MovSxd { dest; src } ->
    let src' = op_prop src t sub in
    MovSxd { dest; src = src' }
  | MovFrom { dest; size; src } ->
    let src' = op_prop src t sub in
    MovFrom { dest; size; src = src' }
  | MovTo { dest; size; src } ->
    let dest' = op_prop dest t sub in
    let src' = op_prop src t sub in
    MovTo { dest = dest'; size; src = src' }
  | Set { typ; src } ->
    let src' = op_prop src t sub in
    Set { typ; src = src' }
  | Cmp { size; lhs; rhs } ->
    let lhs' = op_prop lhs t sub in
    let rhs' = op_prop rhs t sub in
    Cmp { size; lhs = lhs'; rhs = rhs' }
  | LoadFromStack _ ->
    failwith "propagate: impossible, LoadFromStack temps never get propagated"
  | Call call ->
    let args_overflow =
      List.map call.args_overflow ~f:(fun (op, sz) -> op_prop op t sub, sz)
    in
    Call { call with args_overflow }
  | LeaArray { dest; base; index; scale; offset } ->
    LeaArray
      { dest = op_prop dest t sub
      ; base = op_prop base t sub
      ; index = op_prop index t sub
      ; scale
      ; offset
      }
  | _ -> instr
;;

let instr_prop (instr : SSA.instr) (t : Temp.t) (sub : AS.operand) : SSA.instr =
  match instr with
  | ASInstr asinstr -> ASInstr (asinstr_prop op_prop asinstr t sub)
  | Phi { self; alt_selves } ->
    let alt_selves' = List.map alt_selves ~f:(fun (l, op) -> l, op_prop op t sub) in
    Phi { self; alt_selves = alt_selves' }
  | Nop -> Nop
;;

let op_prop_from_tbl (oper_tbl : AS.operand OperandTbl.t)
    : AS.operand -> Temp.t -> AS.operand -> AS.operand
  =
 fun op _ _ ->
  match OperandTbl.find oper_tbl op with
  | None -> op
  | Some new_op -> new_op
;;

(* t and sub are ignored in the end *)
let instr_prop_with_tbl
    (instr : SSA.instr)
    (oper_tbl : AS.operand OperandTbl.t)
    (t : Temp.t)
    (sub : AS.operand)
    : SSA.instr
  =
  match instr with
  | ASInstr asinstr -> ASInstr (asinstr_prop (op_prop_from_tbl oper_tbl) asinstr t sub)
  | Phi { self; alt_selves } ->
    let alt_selves' =
      List.map alt_selves ~f:(fun (l, op) -> l, (op_prop_from_tbl oper_tbl) op t sub)
    in
    Phi { self; alt_selves = alt_selves' }
  | Nop -> Nop
;;

let update_tuse (ln : int) = function
  | None ->
    failwith (sprintf "propagate on ln %i:  why isn't this temp documented in tuse?" ln)
  | Some use -> IS.remove use ln
;;

(*_ ************* immediate phi_optimizations ************ *)
let first_phiopt_target ({ self; alt_selves } : SSA.phi) : Temp.t option =
  let opset = List.map alt_selves ~f:snd |> AS.Set.of_list in
  if AS.Set.length opset = 1 && AS.Set.mem opset (AS.Temp self) then Some self else None
;;

let first_phiopt (code : SSA.instr IH.t) (tuse : IS.t TH.t) (i2phi : (int * SSA.phi) list)
    : SSA.instr IH.t * IS.t TH.t
  =
  (*_ update pointer "code" and "tuse" *)
  let targets =
    List.filter_map i2phi ~f:(fun (i, phi) ->
        match first_phiopt_target phi with
        | Some t -> Some (i, t)
        | None -> None)
  in
  (*_ for each t on line i ( before was i:t <- phi(t) )
     - replace code[i] with Nop 
     - delete i from tuse[t] *)
  let () = List.iter targets ~f:(fun (ln, _) -> IH.update code ln ~f:(fun _ -> Nop)) in
  let () =
    List.iter targets ~f:(fun (ln, t) ->
        TH.update tuse t ~f:(fun data ->
            match data with
            | Some use -> IS.remove use ln
            | None -> failwith "propagate: why isn't this temp documented in tuse?"))
  in
  code, tuse
;;

let second_phiopt_target ({ self; alt_selves } : SSA.phi) : (Temp.t * AS.operand) option =
  let opset = AS.Temp self :: List.map alt_selves ~f:snd |> AS.Set.of_list in
  if AS.Set.length opset = 2 (* && AS.Set.mem opset (AS.Temp self) *)
  then (
    let opset' = AS.Set.remove opset (AS.Temp self) in
    let op = AS.Set.to_list opset' |> List.hd_exn in
    Some (self, op))
  else None
;;

(* let third_phiopt_target ({ self; alt_selves } : SSA.phi) : (Temp.t * AS.operand) option = 
  let opset = List.map alt_selves ~f:snd |> AS.Set.of_list in
  failwith "No"
;; *)

(* let second_phiopt
    (code : SSA.instr IH.t)
    (tuse : IS.t TH.t)
    (i2phi : (int * SSA.phi) list)
    : SSA.instr IH.t * IS.t TH.t
  =
  let targets =
    List.filter_map i2phi ~f:(fun (i, phi) ->
        match second_phiopt_target phi with
        | Some tandop -> Some (i, tandop)
        | None -> None)
  in
  let wq = Queue.of_list targets in
  let rec loop () =
    match Queue.dequeue wq with
    | None -> ()
    | Some (ln, (t, sub)) ->
      (*_ replace code[i] with Nop *)
      let () = IH.update code ln ~f:(fun _ -> Nop) in
      (*_ delete i from tuse[t] and tuse[t'] *)
      let () = TH.update tuse t ~f:(update_tuse ln) in
      let () =
        match sub with
        | AS.Temp t' -> TH.update tuse t' ~f:(update_tuse ln)
        | _ -> ()
      in
      (*_ get tuse[t] *)
      let tlines = TH.find_exn tuse t in
      (*_ for each t on line i: (before was i:t <- phi(t, t'))
       - for each ln in tuse[t], update code[ln] with instr_prop 
       - for thosee ln in tuse[t] that are phis, filter those that are *)
      let phi_list =
        List.filter_map (IS.to_list tlines) ~f:(fun ln ->
            (*_ for each ln in tuse[t], update code[ln] with instr_prop *)
            let lncode = IH.find_exn code ln in
            let lncode' = instr_prop lncode t sub in
            let () = IH.update code ln ~f:(fun _ -> lncode') in
            (* filter those ln in tuse[t] that are phis *)
            match lncode with
            | SSA.Phi phi -> Some (ln, phi)
            | _ -> None)
      in
      (*_ update tuse[t'] = tuse[t'] u tuse[t]*)
      let () =
        match sub with
        | Temp t' ->
          let tlines' = TH.find_exn tuse t' in
          TH.update tuse t' ~f:(fun _ -> IS.union tlines tlines')
        | _ -> ()
      in
      (*_ delete t from tuse *)
      let () = TH.remove tuse t in
      (*_ put all lines tuse[t] that are second_targets back onto the queue *)
      let target' =
        List.filter_map phi_list ~f:(fun (i, phi) ->
            match second_phiopt_target phi with
            | Some res -> Some (i, res)
            | _ -> None)
      in
      let () = Queue.enqueue_all wq target' in
      loop ()
  in
  let () = loop () in
  code, tuse
;; *)

let second_phiopt (code : SSA.instr IH.t) (tuse : IS.t TH.t) : SSA.instr IH.t * IS.t TH.t =
  let targets_init =
    IH.filter code ~f:(fun instr ->
        match instr with
        | SSA.Phi _ -> true
        | _ -> false)
    |> IH.keys
  in
  let wq = Queue.of_list targets_init in
  let rec loop () =
    match Queue.dequeue wq with
    | None -> ()
    | Some ln ->
      let instr_phi = IH.find_exn code ln in
      (match instr_phi with
      | SSA.Phi phi ->
        (match second_phiopt_target phi with
        | None -> loop ()
        | Some (t, sub) ->
          let () = IH.update code ln ~f:(fun _ -> Nop) in
          let () =
            match sub with
            | AS.Temp t' -> TH.update tuse t' ~f:(update_tuse ln)
            | _ -> ()
          in
          (*_ get tuse[t] *)
          let tlines =
            match TH.find tuse t with
            | Some lset -> lset
            | None ->
              failwith
                (sprintf
                   "propagate: tuse[%s] is gone before it was deleted"
                   (Temp.name t))
          in
          (*_ for each t on line i: (before was i:t <- const/reg/t')
     - for each ln in tuse[t], update code[ln] with instr_prop 
     - collect ln's in tuse[t] that are asinstr *)
          let phi_list =
            List.filter_map (IS.to_list tlines) ~f:(fun ln ->
                (*_ for each ln in tuse[t], update code[ln] with instr_prop *)
                let lncode = IH.find_exn code ln in
                let lncode' = instr_prop lncode t sub in
                let () = IH.update code ln ~f:(fun _ -> lncode') in
                (* filter those ln in tuse[t] that are phis *)
                match lncode with
                | SSA.Phi phi -> Some (ln, phi)
                | _ -> None)
          in
          (*_ update tuse[t'] = tuse[t'] u tuse[t]*)
          let () =
            match sub with
            | Temp t' ->
              let tlines' = TH.find_exn tuse t' in
              TH.update tuse t' ~f:(fun _ -> IS.union tlines tlines')
            | _ -> ()
          in
          (*_ delete t from tuse *)
          let () = TH.remove tuse t in
          (*_ put all lines tuse[t] that are targets back onto the queue *)
          let target' =
            List.filter_map phi_list ~f:(fun (i, phi) ->
                match second_phiopt_target phi with
                | Some _ -> Some i
                | _ -> None)
          in
          let () = Queue.enqueue_all wq target' in
          loop ())
      | _ -> loop ())
  in
  let () = loop () in
  code, tuse
;;

let fspace_phiopt (fspace : SSA.fspace) : SSA.fspace =
  let i2phies =
    IH.filter_map fspace.code ~f:(fun code ->
        match code with
        | SSA.Phi phi -> Some phi
        | _ -> None)
    |> IH.to_alist
  in
  (*_ snd phi optimization: update code and tuse *)
  let code, tuse = second_phiopt fspace.code fspace.tuse in
  (*_ fst phi optimization: update code and tuse *)
  let code, tuse = first_phiopt code tuse i2phies in
  { fspace with code; tuse }
;;

let phiopt (prog : SSA.program) : SSA.program = List.map prog ~f:fspace_phiopt

(* copy and constant propagation all in one function *)

let target_instr : AS.instr -> (Temp.t * AS.operand) option = function
  | AS.Mov { dest = AS.Temp d; src = AS.Imm c; _ } -> Some (d, AS.Imm c)
  | AS.Mov { dest = AS.Temp d; src = AS.Temp t; _ } -> Some (d, AS.Temp t)
  | AS.PureBinop { dest = AS.Temp d; lhs = AS.Imm c1; rhs = AS.Imm c2; op; _ } ->
    let c =
      match op with
      | AS.Add -> Int64.( + ) c1 c2
      | AS.Sub -> Int64.( - ) c1 c2
      | AS.Mul -> Int64.( * ) c1 c2
      | AS.BitAnd -> Int64.bit_and c1 c2
      | AS.BitOr -> Int64.bit_or c1 c2
      | AS.BitXor -> Int64.bit_xor c1 c2
    in
    (match Int32.of_int64 c with
    | None -> None
    | Some _ -> Some (d, AS.Imm c))
  (* | AS.EfktBinop { dest = AS.Temp d; lhs = AS.Imm c1; rhs = AS.Imm c2; op; _ } ->
    (match op with
     | AS.Div ->
       if Int64.equal c2 Int64.zero
       then None
       else (
         let c = Int64.( / ) c1 c2 in
         Some (d, AS.Imm c))
     | AS.Mod ->
       if Int64.equal c2 Int64.zero
       then None
       else (
         let c = Int64.( % ) c1 c2 in
         Some (d, AS.Imm c))
     | _ -> None) *)
  | _ -> None
;;

let target (instr : SSA.instr) : (Temp.t * AS.operand) option =
  match instr with
  | SSA.ASInstr asinstr -> target_instr asinstr
  | _ -> None
;;

let propagate_opt (code : SSA.instr IH.t) (tuse : IS.t TH.t) : SSA.instr IH.t * IS.t TH.t =
  let targets_init = IH.keys code in
  let wq = Queue.of_list targets_init in
  let rec loop () =
    match Queue.dequeue wq with
    | None -> ()
    | Some ln ->
      let instr_ssa = IH.find_exn code ln in
      (match target instr_ssa with
      | None -> loop ()
      | Some (t, sub) ->
        (* let () =
           debug_print
             (sprintf
                "propagate %i : %s <- %s\n"
                ln
                (Temp.name t)
                (AS.format_operand sub))
         in *)
        (* replace code[ln] with Nop *)
        let () = IH.update code ln ~f:(fun _ -> Nop) in
        (* let () =
           debug_print
             (sprintf "delete code: %s\n" (SSA.pp_instr ln (IH.find_exn code ln)))
         in *)
        (*_ delete ln from tuse[t'] *)
        let () =
          match sub with
          | AS.Temp t' -> TH.update tuse t' ~f:(update_tuse ln)
          (* debug_print
               (sprintf
                  "check if %i is still in tuse[%s]={%s}\n"
                  ln
                  (Temp.name t')
                  (pp_IS (TH.find_exn tuse t'))) *)
          | _ -> ()
        in
        (*_ get tuse[t] *)
        let tlines =
          match TH.find tuse t with
          | Some lset -> lset
          | None ->
            failwith
              (sprintf "propagate: tuse[%s] is gone before it was deleted" (Temp.name t))
        in
        (*_ for each t on line i: (before was i:t <- const/reg/t')
     - for each ln in tuse[t], update code[ln] with instr_prop 
     - collect ln's in tuse[t] that are asinstr *)
        let instr_list =
          List.map (IS.to_list tlines) ~f:(fun ln ->
              (*_ for each ln in tuse[t], update code[ln] with instr_prop *)
              let lncode = IH.find_exn code ln in
              let lncode' = instr_prop lncode t sub in
              let () = IH.update code ln ~f:(fun _ -> lncode') in
              (* let () =
               debug_print
                 (sprintf
                    ">>> %s ==> %s\n"
                    (SSA.pp_instr ln lncode)
                    (SSA.pp_instr ln lncode'))
             in *)
              (* filter those ln in tuse[t] that are asinstr *)
              ln)
        in
        (*_ update tuse[t'] = tuse[t'] u tuse[t]*)
        let () =
          match sub with
          | Temp t' ->
            let tlines' = TH.find_exn tuse t' in
            TH.update tuse t' ~f:(fun _ -> IS.union tlines tlines')
            (* debug_print
               (sprintf
                  "tuse[%s] is now %s\n"
                  (Temp.name t')
                  (pp_IS (TH.find_exn tuse t'))) *)
          | _ -> ()
        in
        (*_ delete t from tuse *)
        let () = TH.remove tuse t in
        (*_ put all lines tuse[t] that are targets back onto the queue *)
        (* let targets' =
           List.filter_map instr_list ~f:(fun (i, instr) ->
             match target instr with
             | Some _ -> Some i
             | _ -> None)
         in *)
        let () = Queue.enqueue_all wq instr_list in
        loop ())
  in
  let () = loop () in
  code, tuse
;;

let propagate_lea (fspace : SSA.fspace) (code : SSA.instr IH.t) (tuse : IS.t TH.t) =
  let blocks = fspace.block_info in
  (* IGNORE *)
  let itemp = Temp.create () in
  let ioper = AS.Temp itemp in
  (* IGNORE *)
  let () =
    List.iter blocks ~f:(fun b ->
        let lea_tbl : AS.operand InstrTbl.t = InstrTbl.create () in
        let oper_tbl : AS.operand OperandTbl.t = OperandTbl.create () in
        let lines = b.lines in
        (* UPDATE TUSE? *)
        List.iter lines ~f:(fun ln ->
            match IH.find_exn code ln with
            | ASInstr (LeaArray ({ dest; _ } as lea_vals)) ->
              let lea_key : AS.instr = LeaArray { lea_vals with dest = ioper } in
              (match InstrTbl.find lea_tbl lea_key with
              (* if have not seen this lea, then add it to the table *)
              | None ->
                print_endline
                  (sprintf
                     "adding the lea %s on %s"
                     (AS.format_instr lea_key)
                     (Label.format_bt b.label));
                InstrTbl.add_exn lea_tbl ~key:lea_key ~data:dest
              | Some oper ->
                (* if found make it nop *)
                print_endline
                  (sprintf "found the lea again on %s" (Label.format_bt b.label));
                IH.update code ln ~f:(fun _ -> Nop);
                (* add the temp to oper_tbl *)
                OperandTbl.add_exn ~key:dest ~data:oper oper_tbl)
            (* we never update loadfromstack *)
            | ASInstr (LoadFromStack _) -> ()
            | ASInstr instr ->
              let updated_instr =
                instr_prop_with_tbl (ASInstr instr) oper_tbl itemp ioper
              in
              (*  in case updated is different, update the code *)
              (match updated_instr with
              | ASInstr new_instr ->
                if AS.equal_instr instr new_instr
                then ()
                else IH.update code ln ~f:(fun _ -> updated_instr)
              | _ -> failwith "can not happen in propagation propagate_lea ")
            (* update phi anyways *)
            | Phi phi ->
              let updated_instr = instr_prop_with_tbl (Phi phi) oper_tbl itemp ioper in
              IH.update code ln ~f:(fun _ -> updated_instr)
            | Nop -> ()))
  in
  code, tuse
;;

let lea_prop_on = false

let propagate_fspace (fspace : SSA.fspace) : SSA.fspace =
  (* let () = debug_print (sprintf "prop on fspace %s\n" (Symbol.name fspace.fname)) in *)
  let code, tuse = propagate_opt fspace.code fspace.tuse in
  let code, tuse = if lea_prop_on then propagate_lea fspace code tuse else code, tuse in
  { fspace with code; tuse }
;;

let propagate (prog : SSA.program) : SSA.program = List.map prog ~f:propagate_fspace