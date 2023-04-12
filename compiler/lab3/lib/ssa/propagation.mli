module SSA = Ssa
module AS = Assem_l4

val propagate : SSA.program -> SSA.program
val phiopt : SSA.program -> SSA.program

val debug : AS.program -> unit