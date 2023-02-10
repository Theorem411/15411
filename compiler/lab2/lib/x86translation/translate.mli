module AS = Assem

type __operand =
  | Temp of Temp.t
  | Reg of AS.reg
[@@deriving equal]

val translate : AS.instr list -> X86.instr list