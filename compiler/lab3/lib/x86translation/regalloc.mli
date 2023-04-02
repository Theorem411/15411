module AS = Assem_l4
module V = Graph.Vertex
module TM = Temp.Map
module Reg = Register

type color = (int[@deriving sexp, equal, compare])

type reg_or_spill =
  | Reg of Reg.reg_enum
  | Spl of int

val reg_alloc : AS.fspace -> reg_or_spill TM.t