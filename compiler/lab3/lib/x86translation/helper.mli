(* module AS = Assem_l4
module V = Graph.Vertex
module Regalloc = Regalloc
module TM = Temp.Map

type color = (int[@deriving sexp, equal, compare])

val reg_alloc : AS.fspace -> X86.operand AS.Map.t * int

(* val dump_liveness: bool ref *)

(* returns the function enter, exit and return label *)
val get_function_be
:  Symbol.t * (Temp.t * AS.size) list
-> Regalloc.reg_or_spill TM.t
-> color
  -> X86.instr list * X86.instr list * Label.t *)