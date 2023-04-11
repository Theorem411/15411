module SSA = Ssa
val propagate : SSA.program -> SSA.program
val phiopt : SSA.program -> SSA.program