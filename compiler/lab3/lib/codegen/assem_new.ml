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
  | RCX
  | RDX
  | RSI
  | RDI
  | RBX
[@@deriving equal, sexp, compare, enum, hash]

let arg_i_to_reg = function
  | 0 -> EDI
  | 1 -> ESI
  | 2 -> EDX
  | 3 -> ECX
  | 4 -> R8D
  | 5 -> R9D
  | _ -> failwith "args overflow 6"
;;

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

let format_reg r = sexp_of_reg r |> string_of_sexp

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

let format_instr' = function
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
  | Lab l -> "\n.Label " ^ Label.name l
  | Ret -> "ret"
  | Set c ->
    sprintf "%s %s" (c.typ |> sexp_of_set_t |> string_of_sexp) (format_operand c.src)
  | Cmp (l, r) -> sprintf "cmp %s, %s" (format_operand l) (format_operand r)
  | AssertFail -> "call __assert_fail"
  | Call { fname; args_overflow } ->
    sprintf
      "call %s(%s)"
      (Symbol.name fname)
      (List.map args_overflow ~f:(fun t -> Temp.name t ^ ", ") |> String.concat)
  | LoadFromStack ts -> 
    sprintf 
      "loadfromstack {%s}"
      (List.map ts ~f:(fun t -> Temp.name t ^ ", ") |> String.concat)

;;

let format_instr i = (format_instr' i) ^ "\n"

let format_jump_tag = function
  | JRet -> "ret"
  | JCon { jt; jf } -> sprintf "if(%s|%s)" (Label.name jt) (Label.name jf)
  | JUncon l -> Label.name l
;;

let format_block ({ label; block; jump } : block) : string =
  sprintf
    "\n%s:\n%s\n%s\n"
    (Label.name label)
    (List.map block ~f:format_instr |> String.concat)
    (format_jump_tag jump)
;;
let format_program_block (prog_block) =
  let format_fspace_block { fname; args; fdef_block; } = 
    sprintf "%s(%s): \n%s"
    (Symbol.name fname)
    (List.map args ~f:Temp.name |> String.concat)
    (List.map fdef_block ~f:format_block |> String.concat)
  in
  List.map prog_block ~f:format_fspace_block |> String.concat
;;
let format_program prog =
  let format_fspace fspace =
    sprintf
      "%s(%s): \n%s"
      (Symbol.name fspace.fname)
      (List.map fspace.args ~f:Temp.name |> String.concat ~sep:", ")
      (List.map fspace.fdef ~f:format_instr |> String.concat)
  in
  List.map prog ~f:format_fspace |> String.concat
;;

module T = struct
  type t = operand
  [@@deriving compare, equal, sexp]
end
include Comparable.Make(T)