open Core
module AS = Assem_l4
module SSA = Ssa
module IH = SSA.IH
module TH = SSA.TH
module IS = SSA.IS

module T = struct
  type t = Temp.t [@@deriving equal, compare, sexp, hash]
end

(*_ ************  helper function **************** *)
let op_prop (op : AS.operand) (t : Temp.t) (sub : AS.operand) : AS.operand =
  match op with
  | AS.Temp t' -> if Temp.equal t t' then sub else op
  | _ -> op
;;

let asinstr_prop (instr : AS.instr) (t : Temp.t) (sub : AS.operand) : AS.instr =
  match instr with
  | PureBinop { op; size; dest; lhs; rhs } ->
    let lhs' = op_prop lhs t sub in
    let rhs' = op_prop rhs t sub in
    PureBinop { op; size; dest; lhs = lhs'; rhs = rhs' }
  | EfktBinop { op; dest; lhs; rhs } ->
    let lhs' = op_prop lhs t sub in
    let rhs' = op_prop rhs t sub in
    EfktBinop { op; dest; lhs = lhs'; rhs = rhs' }
  | Unop { op; dest } ->
    let dest' = op_prop dest t sub in
    Unop { op; dest = dest' }
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
  | Call _ ->
    failwith
      "not implemented: have problem with 3-assem representation of loading on the stack \
       operands"
  | _ -> instr
;;

let instr_prop (instr : SSA.instr) (t : Temp.t) (sub : AS.operand) : SSA.instr =
  match instr with
  | ASInstr asinstr -> ASInstr (asinstr_prop asinstr t sub)
  | Phi { self; alt_selves } ->
    let alt_selves' = List.map alt_selves ~f:(fun (l, op) -> l, op_prop op t sub) in
    Phi { self; alt_selves = alt_selves' }
  | Nop -> Nop
;;

let update_tuse (ln : int) = function
  | None -> failwith "propagate:  why isn't this temp documented in tuse?"
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
  let opset = List.map alt_selves ~f:snd |> AS.Set.of_list in
  if AS.Set.length opset = 2 && AS.Set.mem opset (AS.Temp self)
  then (
    let opset' = AS.Set.remove opset (AS.Temp self) in
    let op = AS.Set.to_list opset' |> List.hd_exn in
    Some (self, op))
  else None
;;

let second_phiopt
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
  let code, tuse = second_phiopt fspace.code fspace.tuse i2phies in
  (*_ fst phi optimization: update code and tuse *)
  let code, tuse = first_phiopt code tuse i2phies in
  { fspace with code; tuse }
;;

let phiopt (prog : SSA.program) : SSA.program = List.map prog ~f:fspace_phiopt

(* copy and constant propagation all in one function *)

let target (instr : AS.instr) : (Temp.t * AS.operand) option =
  match instr with
  | AS.Mov { dest = AS.Temp t; src; _ } -> Some (t, src)
  | _ -> None
;;

let propagate_opt
  (code : SSA.instr IH.t)
  (tuse : IS.t TH.t)
  (i2code : (int * AS.instr) list)
  : SSA.instr IH.t * IS.t TH.t
  =
  let targets =
    List.filter_map i2code ~f:(fun (i, code) ->
      match target code with
      | Some res -> Some (i, res)
      | _ -> None)
  in
  let wq = Queue.of_list targets in
  let rec loop () =
    match Queue.dequeue wq with
    | None -> ()
    | Some (ln, (t, sub)) ->
      (* replace code[ln] with Nop *)
      let () = IH.update code ln ~f:(fun _ -> Nop) in
      (*_ delete ln from tuse[t] and tuse[t'] *)
      let () = TH.update tuse t ~f:(update_tuse ln) in
      let () =
        match sub with
        | AS.Temp t' -> TH.update tuse t' ~f:(update_tuse ln)
        | _ -> ()
      in
      (*_ get tuse[t] *)
      let tlines = TH.find_exn tuse t in
      (*_ for each t on line i: (before was i:t <- const/reg/t')
       - for each ln in tuse[t], update code[ln] with instr_prop 
       - collect ln's in tuse[t] that are asinstr *)
      let instr_list =
        List.filter_map (IS.to_list tlines) ~f:(fun ln ->
          (*_ for each ln in tuse[t], update code[ln] with instr_prop *)
          let lncode = IH.find_exn code ln in
          let lncode' = instr_prop lncode t sub in
          let () = IH.update code ln ~f:(fun _ -> lncode') in
          (* filter those ln in tuse[t] that are asinstr *)
          match lncode with
          | SSA.ASInstr instr -> Some (ln, instr)
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
        List.filter_map instr_list ~f:(fun (i, instr) ->
          match target instr with
          | Some res -> Some (i, res)
          | _ -> None)
      in
      let () = Queue.enqueue_all wq target' in
      loop ()
  in
  let () = loop () in
  code, tuse
;;

let propagate_fspace (fspace : SSA.fspace) : SSA.fspace =
  let i2code =
    IH.filter_map fspace.code ~f:(fun code ->
      match code with
      | SSA.ASInstr instr -> Some instr
      | _ -> None)
    |> IH.to_alist
  in
  let code, tuse = propagate_opt fspace.code fspace.tuse i2code in
  { fspace with code; tuse }
;;

let propagate (prog : SSA.program) : SSA.program = List.map prog ~f:propagate_fspace
