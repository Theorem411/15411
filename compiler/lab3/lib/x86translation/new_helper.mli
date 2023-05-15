module AS = Assem_l4
module V = Graph.Vertex
module Regalloc = Regalloc
module TM = Temp.Map

(* val dump_liveness: bool ref *)

(* returns the function enter, exit and return label *)
val get_function_be
:  Symbol.t * (Temp.t * AS.size) list
-> Regalloc.reg_or_spill TM.t * (Temp.t -> Regalloc.reg_or_spill)
-> int
  -> X86.instr list * X86.instr list * (X86.instr list * int) * Label.t