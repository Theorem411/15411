open Core
type reg =
  | EAX
  | EDX
  | ECX
  | ESI
  | EDI
  | EBX
  | R8D
  | R9D
  | R10D
  | R11D
  | R12D
  | R13D
  | R14D
  | R15D
  | RBP
  | RSP
  | RCX
  | RDX
  | RSI
  | RDI
  | RBX
[@@deriving equal, sexp, compare, enum, hash]

type operand =
  | Imm of Int32.t
  | Reg of reg
  | Temp of Temp.t
[@@deriving equal, sexp, compare]

type pure_operation =
  | Add
  | Sub
  | Mul
  | BitAnd
  | BitOr
  | BitXor
[@@deriving equal, sexp, compare]

type unary_operation = BitNot [@@deriving equal, sexp, compare]

type efkt_operation =
  | Div
  | Mod
  | ShiftL
  | ShiftR
[@@deriving equal, sexp, compare]

type jump_t =
  | Je (*_ jump if p1 == p2 *)
  (* | Jz  _ jump if p1 == 0 *)
  | Jne (*_ jump if p1 != p2 *)
  (* | Jnz _ jump if p1 != 0 *)
  | Jl (*_ jump if p1 < p2 *)
  (* | Jnge _ jump if NOT p1 >= p2 *)
  | Jge (*_ jump if p1 >= p2 *)
  (* | Jnl _ jump if NOT p1 < p2 *)
  | Jle (*_ jump if p1 <= p2 *)
  (* | Jng _ jump if NOT p1 > p2 *)
  | Jg
(*_ jump if p1 > p2 *)
(* | Jnle _ jump if NOT p1 <= p2 *)
[@@deriving equal, sexp, compare]

(*_ what is potentially missing? 
  - Any parity flag related jumps: e.g., jp, jpe
  - Any unsigned version of the above: e.g. jb, ja
  - Any carry flag related jumps : e.g., jc, jnc
  - Any sign flag related jumps: e.g., js, jns 
  btw disabled jz and jnz because I think they need to be used with test (an
  alternative to cmp in x86)
   *)
type set_t =
  | Sete
  | Setne
  | Setg
  | Setge
  | Setl
  | Setle
[@@deriving equal, sexp, compare]

type instr =
  (* dest <- lhs op rhs *)
  | PureBinop of
      { op : pure_operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  | EfktBinop of
      { op : efkt_operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  | Unop of
      { op : unary_operation
      ; dest : operand
      }
  (* dest <- src *)
  | Mov of
      { dest : operand
      ; src : operand
      }
  (*_ unconditional jump *)
  | Jmp of Label.t
  (*_ conditional jump *)
  | Cjmp of
      { typ : jump_t
      ; l : Label.t
      }
  | Set of
      { typ : set_t
      ; src : operand
      }
  | Ret
  | Lab of Label.t
  | Cmp of operand * operand
  | AssertFail
  (* this is in the third assem *)
  | LoadFromStack of Temp.t list
  | Call of
      { fname : Symbol.t
      ; args_in_regs : reg list
      ; args_overflow : Temp.t list
      }
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string
[@@deriving equal, sexp, compare]

type jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t

type block =
  { label : Label.t
  ; block : instr list
  ; jump : jump_tag_t
  }

type fspace_block =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_block : block list
  }

type fspace =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef : instr list
  }

type program_block = fspace_block list
type program = fspace list

val arg_i_to_reg : int -> reg
val format_reg : reg -> string
val format_instr : instr -> string
val format_program_block : program_block -> string
val format_program : program -> string

include Comparable.S with type t := operand