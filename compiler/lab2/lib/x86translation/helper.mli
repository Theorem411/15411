module AS = Assem_new
module V = Graph.Vertex

type color = int [@deriving sexp, equal, compare]

val get_reg_h:((AS.operand * color) list * (color * X86.operand) list ->
    AS.operand -> X86.operand);;