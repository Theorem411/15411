(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 *)

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
[@@deriving equal, sexp, compare, enum]

type operand =
  | Imm of Int32.t
  | Reg of reg
  | Temp of Temp.t
[@@deriving equal, sexp, compare]

type operation =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
[@@deriving equal, sexp, compare]

type instr =
  (* dest <- lhs op rhs *)
  | Binop of
      { op : operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  (* dest <- src *)
  | Mov of
      { dest : operand
      ; src : operand
      }
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string
[@@deriving equal, sexp, compare]

val format : instr -> string
val reg_enum : reg -> int
val equal_operand : operand -> operand -> bool
