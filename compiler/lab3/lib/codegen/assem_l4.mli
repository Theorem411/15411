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
[@@deriving equal, sexp, compare, enum, hash]

type operand =
  | Imm of Int64.t
  | Reg of reg
  | Temp of Temp.t
[@@deriving equal, sexp, compare, hash]

type pure_operation =
  | Add
  | Sub
  | Mul
  | BitAnd
  | BitOr
  | BitXor
[@@deriving equal, sexp, compare, hash]

type unary_operation = BitNot [@@deriving equal, sexp, compare, hash]

type efkt_operation =
  | Div
  | Mod
  | ShiftL
  | ShiftR
[@@deriving equal, sexp, compare, hash]

type jump_t =
  | Je (*_ jump if p1 == p2 *)
  | Jne (*_ jump if p1 != p2 *)
  | Jl (*_ jump if p1 < p2 *)
  | Jge (*_ jump if p1 >= p2 *)
  | Jle (*_ jump if p1 <= p2 *)
  | Jg
  | Js
  | Jb
[@@deriving equal, sexp, compare, hash]

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
[@@deriving equal, sexp, compare, hash]

type size =
  | L
  | S
[@@deriving equal, sexp, compare, hash]

type instr =
  (* dest <- lhs op rhs *)
  | PureBinop of
      { op : pure_operation
      ; size : size
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
      ; src : operand
      }
  (* dest <- src *)
  | Mov of
      { dest : operand
      ; size : size
      ; src : operand
      }
  | MovSxd of
      { (*_ sign extend temp/reg from 32->64 bit *)
        dest : operand
      ; src : operand
      }
  | MovFrom of
      { dest : operand
      ; size : size
      ; src : operand
      }
  | LeaPointer of
      { dest : operand
      ; size : size
      ; base : operand
      ; offset : int
      }
  | LeaArray of
      { dest : operand (* ; size : size not needed, as by default it 8 *)
      ; base : operand
      ; offset : int
      ; index : operand
      ; scale : int (* can be only 1,2,4,8 *)
      }
  | MovTo of
      { dest : operand
      ; size : size
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
  | Cmp of
      { size : size
      ; lhs : operand
      ; rhs : operand
      }
  | AssertFail
  (* this is in the third assem *)
  | LoadFromStack of (Temp.t * size) list
  | Call of
      { fname : Symbol.t
      ; args_in_regs : (reg * size) list
      ; args_overflow : (operand * size) list
      ; tail_call : bool
      }
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string
  | LLVM_Jmp of Label.t
  | LLVM_Ret of (operand * size) option
  | LLVM_IF of
      { cond : operand
      ; tl : Label.t
      ; fl : Label.t
      }
  | LLVM_Call of
      { dest : (operand * size) option
      ; fname : Symbol.t
      ; args : (operand * size) list
      }
  | LLVM_Cmp of
      { size : size
      ; lhs : operand
      ; rhs : operand
      ; typ : set_t
      ; dest : operand
      }
  | LLVM_Set of
      { size : size
      ; lhs : operand
      ; rhs : operand
      ; typ : set_t
      ; dest : operand
      }
[@@deriving equal, sexp, compare, hash]

type assem_jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t
[@@deriving equal, sexp, compare, hash]

type block =
  { label : Label.bt
  ; block : instr list
  ; jump : assem_jump_tag_t
  ; depth : int
  ; is_empty: bool
  }
[@@deriving equal, sexp, compare, hash]

type fspace =
  { fname : Symbol.t
  ; args : (Temp.t * size) list
  ; fdef_blocks : block list
  ; tmp_cnt : int
  ; ret_size : size option
  }
[@@deriving equal, sexp, compare]

type program = fspace list

val all_regs : reg list
val num_regs : int
val mem_fail_lab : Label.t
val format_jump_tag : assem_jump_tag_t -> string
val format_size : size -> string
val format_reg : reg -> string
val format_instr : instr -> string
val format_program : program -> string
val format_operand : operand -> string
val arg_i_to_reg : int -> reg

include Comparable.S with type t := operand