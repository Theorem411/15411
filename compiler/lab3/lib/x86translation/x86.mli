open Core
module AS = Assem

type operand =
  | Imm of Int32.t
  | Reg of AS.reg
  | Mem of int
[@@deriving equal, compare, sexp]

type operation =
  | Add
  | Addq
  | Sub
  | Subq
  | IMul
  | Mul
  | IDiv
  | Mod
  | Cltd
  | Mov
  | Movl
  | Movq
  | Movzx
  | Pushq
  | Popq
  | And
  | Or
  | Xor
  | Not
  | Cmp
  | Sal
  | Sar
  | Call
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
  | ZeroCommand of { op : operation }
  | Directive of string
  | Cmp of
      { rhs : operand
      ; lhs : operand
      }
  | Lbl of Label.t
  | Jump of
      { op : AS.jump_t option
      ; label : Label.t
      }
  | Set of
      { op : AS.set_t
      ; src : operand
      }
  | Comment of string
  | FunName of string
  | Call of string
  | Ret
[@@deriving equal, compare, sexp]

val pure_to_opr : AS.pure_operation -> operation
val efkt_to_opr : AS.efkt_operation -> operation
val unary_to_opr : AS.unary_operation -> operation
val format : instr -> string
val format_operand : ?quad:bool -> ?word:bool -> operand -> string
val format_list : instr list -> string
val __FREE_REG : operand
val all_available_regs : AS.reg list
val callee_saved : operand -> bool
val caller_saved : operand -> bool
val is_reg : operand -> bool
