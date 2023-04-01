open Core
module AS = Assem_l4
module V = Graph.Vertex
module R = Register

let no_reg_alloc = true
let should_print_reg_map = false

type color = int [@@deriving compare, equal, sexp]

let rbp = X86.Reg { reg = R.RBP; size = 8 }
let rsp = X86.Reg { reg = R.RSP; size = 8 }

type color = (int[@deriving sexp, equal, compare])

(*_ helper functions *)
let __v2op (v : V.t) : AS.operand = 
  match v with 
  | V.T t -> AS.Temp t
  | V.R r -> AS.Reg r
;;
let reg_alloc (fspace : AS.fspace) = 
  let graph = Live.mk_graph_fspace (Block.of_fspace fspace) in
  let v2c = Graph.coloring graph in
  let op2c = List.map v2c ~f:(fun (v, c) -> __v2op v, c) in
  
  failwith "not implemented"
;;