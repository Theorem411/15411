open Core
module Tree = Tree

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

let all_regs =
  [ [ EAX; EDI; ESI; EDX; ECX; R8D; R9D ]; [ RBP; EBX; R12D; R13D; R14D; R15D ] ]
  |> List.concat
;;

let num_regs = List.length all_regs

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
      ; args_overflow : (Temp.t * size) list
      }
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string
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
  }
[@@deriving equal, sexp, compare, hash]

let arg_i_to_reg = function
  | 0 -> EDI
  | 1 -> ESI
  | 2 -> EDX
  | 3 -> ECX
  | 4 -> R8D
  | 5 -> R9D
  | _ -> failwith "args overflow 6"
;;

type fspace =
  { fname : Symbol.t
  ; args : (Temp.t * size) list
  ; fdef_blocks : block list
  }
[@@deriving equal, sexp, compare]

type program = fspace list

(*_ globally defined mem fail label *)
let mem_fail_lab = Label.create ()

(*_ format functions *)
let format_reg r = sexp_of_reg r |> string_of_sexp

let format_operand = function
  | Imm n -> "$" ^ Int64.to_string n
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

let format_size = function
  | L -> "l"
  | S -> "s"
;;

let format_instr' = function
  | PureBinop binop ->
    sprintf
      "%s <-%s- %s %s %s"
      (format_operand binop.dest)
      (format_size binop.size)
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
  | Mov { dest; src; size } ->
    sprintf "%s <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | MovSxd { dest; src } ->
    sprintf "movsxd %s <-- %s" (format_operand dest) (format_operand src)
  | Directive dir -> sprintf "%s" dir
  | Comment comment -> sprintf "/* %s */" comment
  | Jmp l -> "jump" ^ Label.name l
  | Cjmp c -> sprintf "%s %s" (c.typ |> sexp_of_jump_t |> string_of_sexp) (Label.name c.l)
  | Lab l -> ".Label " ^ Label.name l
  | Ret -> "ret"
  | Set c ->
    sprintf "%s %s" (c.typ |> sexp_of_set_t |> string_of_sexp) (format_operand c.src)
  | Cmp { size; lhs; rhs } ->
    sprintf "cmp%s %s, %s" (format_size size) (format_operand lhs) (format_operand rhs)
  | AssertFail -> "call __assert_fail"
  | Call { fname; args_in_regs; args_overflow } ->
    sprintf
      "call %s(%s|%s)"
      (Symbol.name fname)
      (List.map args_in_regs ~f:(fun (r, s) ->
         sprintf "%s%s" (format_reg r) (format_size s))
      |> String.concat ~sep:", ")
      (List.map args_overflow ~f:(fun (t, s) ->
         sprintf "%s%s" (Temp.name t) (format_size s))
      |> String.concat ~sep:", ")
  | LoadFromStack ts ->
    sprintf
      "loadfromstack {%s}"
      (List.map ts ~f:(fun (t, s) -> sprintf "%s%s" (Temp.name t) (format_size s))
      |> String.concat ~sep:", ")
  | MovFrom { dest; size; src } ->
    sprintf "%s <-%s- (%s)" (format_operand dest) (format_size size) (format_operand src)
  | MovTo { dest; size; src } ->
    sprintf "(%s) <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
;;

let format_instr i = format_instr' i ^ "\n"

let format_jump_tag = function
  | JRet -> "ret"
  | JCon { jt; jf } -> sprintf "if(%s|%s)" (Label.name jt) (Label.name jf)
  | JUncon l -> Label.name l
;;

let format_block ({ label; block; jump; depth } : block) : string =
  sprintf
    "\n%s [at depth=%i]:\n%s\n%s\n"
    (Label.name_bt label)
    (depth)
    (List.map block ~f:format_instr |> String.concat)
    (format_jump_tag jump)
;;

let format_program prog_block =
  let format_fspace { fname; args; fdef_blocks } =
    sprintf
      "%s(%s): \n%s"
      (Symbol.name fname)
      (List.map args ~f:(fun (t, s) -> sprintf "%s%s" (Temp.name t) (format_size s))
      |> String.concat)
      (List.map fdef_blocks ~f:format_block |> String.concat)
  in
  List.map prog_block ~f:format_fspace |> String.concat
;;

module T = struct
  type t = operand [@@deriving compare, equal, sexp]
end

include Comparable.Make (T)
