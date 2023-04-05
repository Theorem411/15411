open Core
module AS = Assem_l4
module V = Graph.Vertex
module TM = Temp.Map
module R = Register
module Regalloc = Regalloc

let get_temps_line instr : Temp.t list =
  let all_ops : AS.instr -> AS.operand list = function
    | AS.Mov m -> [ m.dest; m.src ]
    | PureBinop b -> [ b.dest; b.lhs; b.rhs ]
    | EfktBinop b -> [ b.dest; b.lhs; b.rhs ]
    | Unop u -> [ u.dest ]
    | Jmp _ | Cjmp _ | Lab _ | AssertFail | AS.Directive _ | AS.Comment _ | Ret -> []
    | LoadFromStack temps -> List.map temps ~f:(fun (t, _) -> AS.Temp t)
    | Cmp { lhs; rhs; _ } -> [ lhs; rhs ]
    | Set { src; _ } -> [ src; AS.Reg EAX ]
    | Call { args_overflow; _ } -> List.map args_overflow ~f:(fun (t, _) -> AS.Temp t)
    | AS.MovFrom m -> [ m.dest; m.src ]
    | AS.MovTo m -> [ m.dest; m.src ]
    | AS.MovSxd m -> [ m.dest; m.src ]
  in
  List.filter_map (all_ops instr) ~f:(fun i ->
      match i with
      | AS.Temp t -> Some t
      | AS.Reg _ -> None
      | _ -> None)
;;

let get_all_temps (f : AS.fspace) =
  let argtemps = List.map f.args ~f:(fun (t, _) -> t) in
  let instrs =
    List.concat_map ~f:(fun (b : AS.block) : AS.instr list -> b.block) f.fdef_blocks
  in
  let all_temps = List.concat (argtemps :: List.map instrs ~f:get_temps_line) in
  List.dedup_and_sort ~compare:Temp.compare all_temps
;;

let stack_alloc : AS.fspace -> Regalloc.reg_or_spill TM.t * AS.fspace  =
 fun (f : AS.fspace) ->
  let temps = get_all_temps f in
  let points = List.mapi temps ~f:(fun i t -> t, Regalloc.Spl i) in
  TM.of_alist_exn points, f;;
