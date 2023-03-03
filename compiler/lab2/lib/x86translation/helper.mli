module AS = Assem_new
module V = Graph.Vertex

type color = int [@deriving sexp, equal, compare]

val reg_alloc: AS.instr list -> X86.operand AS.Map.t * int