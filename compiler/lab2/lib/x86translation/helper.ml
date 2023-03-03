open Core
module AS = Assem_new
module V = Graph.Vertex

type color = int [@deriving sexp, equal, compare]

let get_reg_h (op2col, col2operand) o =
  let get_color_of_operand op2col (o : AS.operand) =
    (fun (_, c) -> c) (List.find_exn op2col ~f:(fun (o2, _) -> AS.equal_operand o o2))
  in
  let get_reg_of_color col2operand (c : color) =
    (fun (_, r) -> r) (List.find_exn col2operand ~f:(fun (uc, _) -> equal_color uc c))
  in
  match o with
  | AS.Imm n -> X86.Imm n
  | _ -> get_reg_of_color col2operand (get_color_of_operand op2col o)
;;