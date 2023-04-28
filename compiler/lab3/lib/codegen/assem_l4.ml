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
  [ [ EDI; ESI; EDX; ECX; EAX; R8D; R9D ]; [ RBP; EBX; R12D; R13D; R14D; R15D ] ]
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
  | LLVM_NullCheck of
      { dest : operand
      ; size : size
      }
  | LLVM_MovTo of
      { dest : operand
      ; size : size
      ; src : operand
      }
  | LLVM_MovFrom of
      { dest : operand
      ; size : size
      ; src : operand
      }
  | LLVM_ArrayIdxCheck of
      { index : operand
      ; length : operand
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
  ; is_empty : bool
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
  ; tmp_cnt : int
  ; ret_size : size option
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
  | Unop { dest; op; src } ->
    sprintf "%s <-s- %s%s" (format_operand dest) (format_unop op) (format_operand src)
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
  | Call { fname; args_in_regs; args_overflow; tail_call } ->
    sprintf
      "call %s(%s|%s)[tail call - %b]"
      (Symbol.name fname)
      (List.map args_in_regs ~f:(fun (r, s) ->
           sprintf "%s%s" (format_reg r) (format_size s))
      |> String.concat ~sep:", ")
      (List.map args_overflow ~f:(fun (op, s) ->
           sprintf "%s%s" (format_operand op) (format_size s))
      |> String.concat ~sep:", ")
      tail_call
  | LoadFromStack ts ->
    sprintf
      "loadfromstack {%s}"
      (List.map ts ~f:(fun (t, s) -> sprintf "%s%s" (Temp.name t) (format_size s))
      |> String.concat ~sep:", ")
  | MovFrom { dest; size; src } ->
    sprintf "%s <-%s- (%s)" (format_operand dest) (format_size size) (format_operand src)
  | MovTo { dest; size; src } ->
    sprintf "(%s) <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | LeaPointer { dest; base; offset; size } ->
    sprintf
      "%s <- lea: [%s] %s + %d"
      (format_operand dest)
      (format_size size)
      (format_operand base)
      offset
  | LeaArray { dest; base; offset; index; scale } ->
    sprintf
      "%s <- lea: %s + %s * %d + %d"
      (format_operand dest)
      (format_operand base)
      (format_operand index)
      scale
      offset
  | LLVM_Jmp l -> sprintf "LLVM: %s" (Label.name l)
  | LLVM_Ret None -> "ret void"
  | LLVM_Ret (Some (src, ret_size)) ->
    sprintf "ret %s[%s]" (format_operand src) (format_size ret_size)
  | LLVM_Call { dest = None; fname; args } ->
    sprintf
      "LLVM: %s(%s);"
      (Symbol.name fname)
      (String.concat
         ~sep:","
         (List.map args ~f:(fun (r, s) ->
              sprintf "%s[%s]" (format_operand r) (format_size s))))
  | LLVM_Call { dest = Some (dest, ret_size); fname; args } ->
    sprintf
      "LLVM: %s = %s(%s)[%s]"
      (format_operand dest)
      (Symbol.name fname)
      (String.concat
         ~sep:","
         (List.map args ~f:(fun (r, s) ->
              sprintf "%s[%s]" (format_operand r) (format_size s))))
      (format_size ret_size)
  | LLVM_IF { cond; tl; fl } ->
    sprintf
      "LLVM: if %s then %s else %s"
      (format_operand cond)
      (Label.name tl)
      (Label.name fl)
  | LLVM_Cmp { dest; typ; lhs; rhs; size } ->
    sprintf
      "LLVM: %s <- cmp[%s] %s: %s ? %s"
      (format_operand dest)
      (format_size size)
      (typ |> sexp_of_set_t |> string_of_sexp)
      (format_operand lhs)
      (format_operand rhs)
  | LLVM_Set { dest; typ; lhs; rhs; size } ->
    sprintf
      "LLVM: %s <- set(that on before if)[%s] %s: %s ? %s"
      (format_operand dest)
      (format_size size)
      (typ |> sexp_of_set_t |> string_of_sexp)
      (format_operand lhs)
      (format_operand rhs)
  | LLVM_MovFrom { dest; size; src } ->
    sprintf
      "LLVM: %s <-%s- (%s)"
      (format_operand dest)
      (format_size size)
      (format_operand src)
  | LLVM_MovTo { dest; size; src } ->
    sprintf
      "LLVM: (%s) <-%s- %s"
      (format_operand dest)
      (format_size size)
      (format_operand src)
  | LLVM_NullCheck { dest; size } -> sprintf "LLVMAAA: Fail if NULL (%s)[%s]" (format_operand dest) (format_size size)
  | LLVM_ArrayIdxCheck { index; length } ->
    sprintf
      "LLVM: Fail if %s is not 0 <= %s < length"
      (format_operand index)
      (format_operand length)
;;

let format_instr i = format_instr' i

let format_jump_tag = function
  | JRet -> "ret"
  | JCon { jt; jf } -> sprintf "if(%s|%s)" (Label.name jt) (Label.name jf)
  | JUncon l -> Label.name l
;;

let format_block ({ label; block; jump; depth; is_empty } : block) : string =
  sprintf
    "\n%s [at depth=%i]:\n%s\njump=%s\n[%b]"
    (Label.name_bt label)
    depth
    (List.map block ~f:format_instr |> String.concat ~sep:"\n")
    (format_jump_tag jump)
    is_empty
;;

let format_program prog_block =
  let format_fspace { fname; args; fdef_blocks; tmp_cnt; ret_size : size option } =
    sprintf
      "%s(%s): \n%s [uses %d temps, returns %s]"
      (Symbol.name fname)
      (List.map args ~f:(fun (t, s) -> sprintf "%s%s" (Temp.name t) (format_size s))
      |> String.concat)
      (List.map fdef_blocks ~f:format_block |> String.concat)
      tmp_cnt
      (Option.value_map ret_size ~default:"void" ~f:format_size)
  in
  List.map prog_block ~f:format_fspace |> String.concat
;;

module T = struct
  type t = operand [@@deriving compare, equal, sexp]
end

include Comparable.Make (T)
