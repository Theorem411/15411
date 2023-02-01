(*_ open Core 
(* let rec live_analysis (assem : Assem.instr list) : TS.t list = 
  let f (instr : Assem.instr) (res : TS.t list) : TS.t list = 
    match instr with 
        Assem.Binop binop -> (
          let res' = (match binop.dest with Assem.Reg r -> Set.remove res (Assem.reg_name r)| Assem.Temp t -> Set.remove res (Temp.name t) | _ -> res) in

        )

      | Assem.Mov mv -> res
      | _ -> res
  in
  List.fold_right assem ~f:f ~init:[] 
;; *)
let is_reg_or_temp (opd: Assem.operand) = 
  match opd with 
      Assem.Reg reg -> Some (Assem.Reg reg)
    | Assem.Temp temp -> Some (Assem.Temp temp)
    | _ -> None
;;
(*blahs*)
let live_in_line = function
      Assem.Binop binop -> ( 
        let res = (match (is_reg_or_temp binop.lhs) with 
              Some op -> []
            | None -> [])
        in [])
    | Assem.Mov mv -> []
    | _ -> []
;;
let rec live_analysis (assem : Assem.instr list) : Assem.instr list list =  *)
