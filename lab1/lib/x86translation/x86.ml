open Core
module AS = Assem

type operand =
  | Imm of Int32.t
  | X86Reg of AS.reg
  | Mem of int
[@@deriving equal, compare, sexp]

let __format_reg = function
  | AS.EAX -> "%eax"
  | AS.EBX -> "%ebx"
  | AS.ECX -> "%ecx"
  | AS.EDX -> "%edx"
  | AS.ESI -> "%esi"
  | AS.EDI -> "%edi"
  | AS.R8D -> "%r8d"
  | AS.R9D -> "%r9d"
  | AS.R10D -> "%r10d"
  | AS.R11D -> "%r11d"
  | AS.R12D -> "%r12d"
  | AS.R13D -> "%r13d"
  | AS.R14D -> "%r14d"
  | AS.R15D -> "%r15d"
;;


(* Mem n means that the varialbe is in -n(%rbp) *)
let format_operand = function
  | Imm n -> "$" ^ Int32.to_string n
  | X86Reg r -> __format_reg r
  | Mem n -> "-" ^ string_of_int n ^ "(%rbp)"
;;

type operation =
  | Add
  | Sub
  | Mul
  | IDiv
  | Mod
  | Mov
  | CLTD
  [@@deriving equal, compare, sexp]

let format_operation = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mul -> "imull"
  | IDiv -> "idivl"
  | Mov -> "movl"
  | CLTD -> "cltd"
  | _ -> raise (Failure "no such operation allowed")
;;

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

let format = function
  | BinCommand binop ->
    sprintf
      "\t%s\t%s, %s"
      (format_operation binop.op)
      (format_operand binop.src)
      (format_operand binop.dest)
  | UnCommand binop ->
    sprintf "\t%s\t%s" (format_operation binop.op) (format_operand binop.src)
  | Zero z -> sprintf "\t%s" (format_operation z.op)
  | Directive dir -> sprintf "\t%s" dir
  | Comment comment -> sprintf "/* %s */" comment
  | FunName s -> sprintf "%s" s
  | Ret -> sprintf "\t%s" "ret"
;;

let to_opr = function
  | AS.Add -> Add
  | AS.Sub -> Sub
  | AS.Mul -> Mul
  | AS.Div -> IDiv
  | AS.Mod -> Mod
;;

let __FREE_REG = X86Reg AS.R11D

