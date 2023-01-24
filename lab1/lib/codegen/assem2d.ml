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

 type reg = EAX
 [@@deriving equal]
 
 type operand2D =
   | Imm of Int32.t
   | Reg of reg
   | Temp of Temp.t
   [@@deriving equal]
 
 type operation2D =
   | Add
   | Sub
   | Mul
   | Div
   | Mod
   | Mov
 
 type instr2D =
   | Command of
       { op : operation2D
       ; dest : operand2D
       ; src : operand2D
       }
   | Directive of string
   | Comment of string
 
 (* functions that format assembly output *)
 
 let format_reg = function
   | EAX -> "%eax"
 ;;
 
 let format_command = function
   | Add -> "+"
   | Sub -> "-"
   | Mul -> "*"
   | Div -> "/"
   | Mod -> "%"
   | Mov -> "mov"
 ;;

 let format = function
   | Add -> "+"
   | Sub -> "-"
   | Mul -> "*"
   | Div -> "/"
   | Mod -> "%"
   | Mov -> "mov"
 ;;

(*  
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
  *)