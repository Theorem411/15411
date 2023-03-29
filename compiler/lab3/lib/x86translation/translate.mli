module AS = Assem_l4

type fspace = X86.instr list
type program = fspace list

val translate : AS.program -> mfail:Label.t -> X86.instr list list
val get_string_list: program -> X86.instr list