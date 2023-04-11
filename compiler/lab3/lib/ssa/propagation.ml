open Core
module AS = Assem_l4
module SSA = Ssa
module IH = Hashtbl.Make (Int)
module T = struct
  type t = Temp.t [@@deriving equal, compare, sexp, hash]
end
module TH = Hashtbl.Make (T)

module IComp = Comparable.Make (Int)
module IS = IComp.Set
(*_ ************* immediate phi_optimizations ************ *)
let fst_phiopt ({ self; alt_selves } : SSA.phi) : Temp.t option =
  let opset = List.map alt_selves ~f:snd |> AS.Set.of_list in
  if AS.Set.length opset = 1 && AS.Set.mem opset (AS.Temp self) then Some self else None
;;

let snd_phiopt ({ self; alt_selves } : SSA.phi) : (Temp.t * AS.operand) option =
  let opset = List.map alt_selves ~f:snd |> AS.Set.of_list in
  if AS.Set.length opset = 2 && AS.Set.mem opset (AS.Temp self)
  then (
    let opset' = AS.Set.remove opset (AS.Temp self) in
    let op = AS.Set.to_list opset' |> List.hd_exn in
    Some (self, op))
  else None
;;

let fspace_phiopt ({ fname; args; code; block_info; tuse } : SSA.fspace) : SSA.fspace =
  let i2phies =
    IH.filter_map code ~f:(fun code ->
      match code with
      | SSA.Phi phi -> Some phi
      | _ -> None)
    |> IH.to_alist
  in
  (*_ 1st phi optimization 
        i: x <- phi(x,x,...,x)
  - replace line i with Nop
  - delete i from tuse(x)
  *)
  (* let fst_opt =
    (*_ line number of 1st type of phi optimization: remove them from tuse *)
    List.filter_map i2phies ~f:(fun (i, phi) ->
      match fst_phiopt phi with
      | Some t -> Some (i, t)
      | None -> None)
  in
  let () = List.iter fst_opt ~f:(fun (i, _) -> IH.update code i ~f:(fun _ -> Nop)) in
  let () =
    List.iter fst_opt ~f:(fun (i, t) ->
      TH.update tuse t ~f:(fun uses ->
        match uses with
        | None -> failwith "ssa: phi-opt encounter a tuse error"
        | Some set -> IS.remove set i))
  in *)
  (*_ 2nd phi optimization: use a queue 
        x <- phi(x,x,y,y,y) 
  - replace line i with nop
  - go to each line in tuse(x), replace x by y
  - update tuse(y) = (tuse(x) u tuse(y)) \ i
  - delete x from tuse
  *)

  failwith "no"
;;

let phiopt (prog : SSA.program) : SSA.program = List.map prog ~f:fspace_phiopt
(*_ ************  helper function **************** *)

let instr_prop (instr : SSA.instr) (t : Temp.t) (op : AS.operand) : SSA.instr =
  match instr with
  | ASInstr instr -> failwith "no"
  | Phi { self; alt_selves } -> failwith "no"
  | Nop -> Nop
;;


let propagate (prog : SSA.program) : SSA.program = 
  (* 0. do a immediate phi-optimization *)
  (* 1. constant/copy propagation *)
  (* 2. do another phi-optimization *)
  failwith "no"
;;
