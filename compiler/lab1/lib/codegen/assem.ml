(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *)

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
[@@deriving equal, sexp, compare]

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
  | Binop of
      { op : operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  | Mov of
      { dest : operand
      ; src : operand
      }
  | Directive of string
  | Comment of string
[@@deriving equal, sexp, compare]

(* functions that format assembly output *)

let format_reg = function
  | EAX -> "%eax"
  | EBX -> "%ebx"
  | ECX -> "%ecx"
  | EDX -> "%edx"
  | ESI -> "%esi"
  | EDI -> "%edi"
  | R8D -> "%r8d"
  | R9D -> "%r9d"
  | R10D -> "%r10d"
  | R11D -> "%r11d"
  | R12D -> "%r12d"
  | R13D -> "%r13d"
  | R14D -> "%r14d"
  | R15D -> "%r15d"
  | RBP -> "%rbp"
  | RSP -> "%rsp"
;;

let format_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
;;

let format_operand = function
  | Imm n -> "$" ^ Int32.to_string n
  | Temp t -> Temp.name t
  | Reg r -> format_reg r
;;

let format = function
  | Binop binop ->
    sprintf
      "%s <-- %s %s %s"
      (format_operand binop.dest)
      (format_operand binop.lhs)
      (format_binop binop.op)
      (format_operand binop.rhs)
  | Mov mv -> sprintf "%s <-- %s" (format_operand mv.dest) (format_operand mv.src)
  | Directive dir -> sprintf "%s" dir
  | Comment comment -> sprintf "/* %s */" comment
;;
