open Core
module AS = Assem_l4
module R = Register

type operand =
  | Imm of Int32.t
  | Stack of int
  | Reg of R.reg
[@@deriving equal, compare, sexp]

type mem =
  | Mem of
      { disp : int option
      ; base_reg : R.reg
      ; idx_reg : R.reg option
      ; scale : int option
      }
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
  | Cqde
  | Mov
  | Movl
  | Movq
  | Movzx
  | Movsx
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
  | Test
  | Div
[@@deriving equal, compare, sexp]

type size =
  | Q
  | L
[@@deriving equal, compare, sexp]

val to_size : AS.size -> size

type instr =
  | BinCommand of
      { op : operation
      ; dest : operand
      ; src : operand
      ; size : size
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
      ; size : size
      }
  | Test of
      { rhs : operand
      ; lhs : operand
      ; size : size
      }
  | Lea of
      { dest : operand
      ; src : mem
      ; size : size
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
  | MovFrom of
      { dest : operand
      ; size : size
      ; src : operand
      }
  | MovTo of
      { dest : operand
      ; size : size
      ; src : operand
      }
  | Movsxd of
      { dest : operand
      ; src : operand
      }
[@@deriving equal, compare, sexp]

val pure_to_opr : AS.pure_operation -> operation
val efkt_to_opr : AS.efkt_operation -> operation
val unary_to_opr : AS.unary_operation -> operation
val format : instr -> string
val format_list : instr list -> string
val get_free : size -> operand
val get_memfree : size -> operand
val all_available_regs : AS.reg list
val callee_saved : operand -> bool
val caller_saved : operand -> bool
val is_reg : operand -> bool
val as_to_reg_enum : AS.reg -> R.reg_enum
