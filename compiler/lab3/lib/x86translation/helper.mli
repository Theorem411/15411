module AS = Assem
module V = Graph.Vertex

type color = (int[@deriving sexp, equal, compare])

val reg_alloc : AS.fspace -> X86.operand AS.Map.t * int

(* val dump_liveness: bool ref *)

(* returns the function enter, exit and return label *)
val get_function_be
  :  Symbol.t * Temp.t list * AS.instr list
  -> X86.operand AS.Map.t
  -> color
  -> X86.instr list * X86.instr list * Label.t