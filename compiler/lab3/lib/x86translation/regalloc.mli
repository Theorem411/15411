module AS = Assem_l4
module V = Graph.Vertex
module TM = Temp.Map
module R = Register

type color = (int[@deriving sexp, equal, compare])

type reg_or_spill =
  | Reg of R.reg_enum
  | Spl of int

val reg_alloc : AS.fspace -> reg_or_spill TM.t
val mem_count : reg_or_spill TM.t -> int
val caller_save : reg_or_spill TM.t -> R.reg_enum list
val callee_save : reg_or_spill TM.t -> R.reg_enum list
val asr2renum : AS.reg -> R.reg_enum 
val pp_temp_map : reg_or_spill TM.t -> string