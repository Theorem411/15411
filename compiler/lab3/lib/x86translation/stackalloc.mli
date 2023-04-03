module AS = Assem_l4
module V = Graph.Vertex
module TM = Temp.Map
module R = Register
module Regalloc = Regalloc

val stack_alloc : AS.fspace -> Regalloc.reg_or_spill TM.t