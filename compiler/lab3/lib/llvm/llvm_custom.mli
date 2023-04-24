type program

module SSA = Ssa

val create : SSA.program -> program
val pp_program : program -> string
val get_pre : string -> string
val get_post : string -> string
