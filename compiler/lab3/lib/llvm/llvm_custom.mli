type program

module SSA = Ssa

val create : SSA.program -> program
val format_program : program -> string
