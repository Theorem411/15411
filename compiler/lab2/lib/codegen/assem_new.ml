open Core
module Tree = Tree_new
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
  | Jnge (*_ jump if NOT p1 >= p2 *)
  | Jge (*_ jump if p1 >= p2 *)
  | Jnl (*_ jump if NOT p1 < p2 *)
  | Jle (*_ jump if p1 <= p2 *)
  | Jng (*_ jump if NOT p1 > p2 *)
  | Jg (*_ jump if p1 > p2 *)
  | Jnle (*_ jump if NOT p1 <= p2 *)
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
  | Lab of Label.t
  | Cmp of operand * operand
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string
  [@@deriving equal, sexp, compare]