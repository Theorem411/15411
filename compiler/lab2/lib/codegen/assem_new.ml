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

let format_reg r = sexp_of_reg r |> string_of_sexp

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

type unary_operation = BitNot [@@deriving equal, sexp, compare]

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
  | Jg (*_ jump if p1 > p2 *)
(* | Jnle _ jump if NOT p1 <= p2 *)
[@@deriving equal, sexp, compare]

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
  | Lab of Label.t
  | Cmp of operand * operand
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string
[@@deriving equal, sexp, compare]

let format_operand = function
  | Imm n -> "$" ^ Int32.to_string n
  | Temp t -> Temp.name t
  | Reg r -> format_reg r
;;

let format_pure_operation = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | BitAnd -> "&"
  | BitXor -> "^"
  | BitOr -> "|"
;;

let format_efkt_operation = function
  | Div -> "/"
  | Mod -> "%"
  | ShiftL -> "<<"
  | ShiftR -> ">>"
;;

let format_unop = function
  | BitNot -> "~"
;;

let format = function
  | PureBinop binop ->
    sprintf
      "%s <-- %s %s %s"
      (format_operand binop.dest)
      (format_operand binop.lhs)
      (format_pure_operation binop.op)
      (format_operand binop.rhs)
  | EfktBinop binop ->
    sprintf
      "%s <-- %s %s %s"
      (format_operand binop.dest)
      (format_operand binop.lhs)
      (format_efkt_operation binop.op)
      (format_operand binop.rhs)
  | Unop unop ->
    sprintf
      "%s <-- %s%s"
      (format_operand unop.dest)
      (format_unop unop.op)
      (format_operand unop.dest)
  | Mov mv -> sprintf "%s <-- %s" (format_operand mv.dest) (format_operand mv.src)
  | Directive dir -> sprintf "%s" dir
  | Comment comment -> sprintf "/* %s */" comment
  | Jmp l -> "jump" ^ Label.name l
  | Cjmp c -> sprintf "%s %s" (c.typ |> sexp_of_jump_t |> string_of_sexp) (Label.name c.l)
  | Lab l -> "\n.Label %s" ^ Label.name l
  | Set c ->
    sprintf "%s %s" (c.typ |> sexp_of_set_t |> string_of_sexp) (format_operand c.src)
  | Cmp (l, r) -> sprintf "cmp %s, %s" (format_operand l) (format_operand r)
;;