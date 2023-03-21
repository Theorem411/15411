module AS = Assem

type fspace = X86.instr list
type program = fspace list

val translate : AS.program -> program
val get_string_list: program -> X86.instr list