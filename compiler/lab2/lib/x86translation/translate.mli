module AS = Assem_new

type fspace =
  { fname : Symbol.t
  ; fdef : X86.instr list
  }
type program = fspace list

val translate : AS.program -> program
val pp_fspace: fspace -> string
val pp_program: program -> string