open Core
module AS = Assem

type operation =
  | Add
  | Sub
  | Subq
  | Addq
  | Mul
  | IDiv
  | Mod
  | Mov
  | Pushq
  | Movq
  | Popq
  | CLTD
  [@@deriving equal, compare, sexp]

type operand =
  | Imm of Int32.t
  | X86Reg of AS.reg
  | Mem of int
  [@@deriving equal, compare, sexp]

type instr =
  | BinCommand of
      { op : operation
      ; dest : operand
      ; src : operand
      }
  | UnCommand of
      { op : operation
      ; src : operand
      }
  | Zero of { op : operation }
  | Directive of string
  | Comment of string
  | FunName of string
  | Ret
  [@@deriving equal, compare, sexp]

val to_opr : AS.operation -> operation
val format : instr -> string
val __FREE_REG : operand
val all_available_regs: AS.reg list
val callee_saved: operand -> bool
val caller_saved: operand -> bool
val is_reg: operand -> bool
