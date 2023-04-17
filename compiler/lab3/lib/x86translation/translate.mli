module AS = Assem_l4

type fspace = X86.instr list
type program = fspace list

val translate : AS.program -> mfail:Label.t -> unsafe:bool -> X86.instr list list
val get_string_list : program -> X86.instr list
val set_tail_off : bool -> unit
val set_strength_off : bool -> unit
val set_block_algn_off : bool -> unit
