open Core
module AS = Assem_l4
module R = Register

let as_to_reg_enum : AS.reg -> R.reg_enum = function
  | AS.EAX -> R.EAX
  | AS.EDX -> R.EDX
  | AS.ECX -> R.ECX
  | AS.ESI -> R.ESI
  | AS.EDI -> R.EDI
  | AS.EBX -> R.EBX
  | AS.R8D -> R.R8D
  | AS.R9D -> R.R9D
  | AS.R10D -> R.R10D
  | AS.R11D -> R.R11D
  | AS.R12D -> R.R12D
  | AS.R13D -> R.R13D
  | AS.R14D -> R.R14D
  | AS.R15D -> R.R15D
  | AS.RBP -> R.RBP
  | AS.RSP -> R.RSP
  | AS.RCX -> R.RCX
  | AS.RDX -> R.RDX
  | AS.RSI -> R.RSI
  | AS.RDI -> R.RDI
  | AS.RBX -> R.RBX
;;

type operand =
  | Imm of Int32.t
  | Stack of int
  | Reg of R.reg
[@@deriving equal, compare, sexp]

(* Stack n means that the varialbe is in -n(%rbp) *)
let format_operand = function
  | Imm n -> "$" ^ Int32.to_string n
  | Reg r -> R.format_reg r
  | Stack n -> string_of_int n ^ "(%rsp)"
;;

type mem =
  | Mem of
      { disp : int option
      ; base_reg : R.reg
      ; idx_reg : R.reg option
      ; scale : int option
      }
[@@deriving equal, compare, sexp]

type operation =
  | Add
  | Addq
  | Sub
  | Subq
  | IMul
  | Mul
  | IDiv
  | Mod
  | Cltd
  | Cqde
  | Mov
  | Movl
  | Movq
  | Movzx
  | Movsx
  | Pushq
  | Popq
  | And
  | Or
  | Xor
  | Not
  | Cmp
  | Sal
  | Sar
  | Call
  | Test
  | Div
  | Movsl
[@@deriving equal, compare, sexp]

type size =
  | Q
  | L
[@@deriving equal, compare, sexp]

let to_size = function
  | AS.L -> Q
  | AS.S -> L
;;

let format_size = function
  | Q -> "q"
  | L -> "l"
;;

let format_operation = function
  | Add -> "add"
  | Sub -> "sub"
  | Addq -> "addq"
  | Subq -> "subq"
  | Mul -> "imul"
  | IDiv -> "idivl"
  | Mov -> "mov"
  | Cltd -> "cltd"
  | Cqde -> "cdqe"
  | Pushq -> "pushq"
  | Popq -> "popq"
  | Movq -> "movq"
  | IMul -> "imul"
  | Movl -> "movl"
  | Movzx -> "movzx"
  | Movsx -> "movsx"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "notl"
  | Cmp -> "cmp"
  | Mod -> "mod"
  | Sal -> "sal"
  | Sar -> "sar"
  | Call -> "call"
  | Test -> "test"
  | Div -> "div"
  | Movsl -> "movsl"
;;

let format_jump j =
  match j with
  | None -> "jmp"
  | Some x ->
    (match x with
    | AS.Je -> "je" (*_ jump if p1 == p2 *)
    | AS.Jne -> "jne" (*_ jump if p1 != p2 *)
    | AS.Jl -> "jl" (*_ jump if p1 < p2 *)
    | AS.Jge -> "jge" (*_ jump if p1 >= p2 *)
    | AS.Jle -> "jle" (*_ jump if p1 <= p2 *)
    | AS.Jg -> "jg" (*_ jump if p1 > p2 *)
    | AS.Js -> "js"
    | AS.Jb -> "jb")
;;

let format_set = function
  | AS.Sete -> "sete"
  | AS.Setne -> "setne"
  | AS.Setg -> "setg"
  | AS.Setge -> "setge"
  | AS.Setl -> "setl"
  | AS.Setle -> "setle"
;;

let integer_formatting (d : int) = if d < 0 then "-", -d else "+", d

let format_mem = function
  | Mem { disp; base_reg; idx_reg; scale } ->
    (match idx_reg, scale, disp with
    | Some idx, Some sc, Some disp ->
      let sign1, sc = integer_formatting sc in
      let sign2, disp = integer_formatting disp in
      sprintf
        "[%s %s %d*%s %s %d]"
        (R.format_reg base_reg)
        sign1
        sc
        (R.format_reg idx)
        sign2
        disp
    | Some idx, Some sc, None ->
      let sign1, sc = integer_formatting sc in
      sprintf "[%s %s %d*%s]" (R.format_reg base_reg) sign1 sc (R.format_reg idx)
    | Some _, None, _ -> failwith "no scale when idx is at memory"
    | None, Some _, _ -> failwith "no index register when given scale"
    | None, None, Some disp -> sprintf "%d(%s)" disp (R.format_reg base_reg)
    | None, None, None -> sprintf "[%s]" (R.format_reg base_reg))
;;

type instr =
  | BinCommand of
      { op : operation
      ; dest : operand
      ; src : operand
      ; size : size
      }
  | UnCommand of
      { op : operation
      ; src : operand
      }
  | ZeroCommand of { op : operation }
  | Directive of string
  | Cmp of
      { rhs : operand
      ; lhs : operand
      ; size : size
      }
  | Test of
      { rhs : operand
      ; lhs : operand
      ; size : size
      }
  | Lea of
      { dest : operand
      ; src : mem
      ; size : size
      }
  | Lbl of Label.t
  | Jump of
      { op : AS.jump_t option
      ; label : Label.t
      }
  | Set of
      { op : AS.set_t
      ; src : operand
      }
  | Comment of string
  | FunName of string
  | Call of string
  | Ret
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
  | Movsxd of
      { dest : operand
      ; src : operand
      }
[@@deriving equal, compare, sexp]

let format = function
  | BinCommand { op = (Addq | Subq) as bop; src = s; dest = d; _ } ->
    sprintf "\t%s\t%s, %s" (format_operation bop) (format_operand s) (format_operand d)
  | BinCommand { op = (Sal | Sar | Movzx) as bop; src = s; dest = d; _ } ->
    sprintf "\t%s\t%s, %s" (format_operation bop) (format_operand s) (format_operand d)
  | BinCommand binop ->
    sprintf
      "\t%s%s\t%s, %s"
      (format_operation binop.op)
      (format_size binop.size)
      (format_operand binop.src)
      (format_operand binop.dest)
  | MovTo binop ->
    sprintf
      "\tmov%s\t%s, (%s)"
      (format_size binop.size)
      (format_operand binop.src)
      (format_operand binop.dest)
  | Movsxd binop ->
    sprintf "\tmovsxd\t%s, %s" (format_operand binop.src) (format_operand binop.dest)
  | MovFrom binop ->
    sprintf
      "\tmov%s\t(%s), %s"
      (format_size binop.size)
      (format_operand binop.src)
      (format_operand binop.dest)
  | UnCommand { op = (Pushq | Popq) as unop; src = s } ->
    sprintf "\t%s\t%s" (format_operation unop) (format_operand s)
  | UnCommand unop ->
    sprintf "\t%s\t%s" (format_operation unop.op) (format_operand unop.src)
  | ZeroCommand z -> sprintf "\t%s" (format_operation z.op)
  | Directive dir -> sprintf "%s" dir
  | Comment comment -> sprintf "/* %s */" comment
  | FunName s -> sprintf "%s:" s
  | Ret -> sprintf "\t%s" "ret"
  | Cmp { rhs; lhs; size } ->
    (match rhs with
    | _ ->
      sprintf
        "\tcmp%s\t%s, %s"
        (format_size size)
        (format_operand rhs)
        (format_operand lhs))
  | Test { rhs; lhs; size } ->
    sprintf
      "\ttest%s\t%s, %s"
      (format_size size)
      (format_operand lhs)
      (format_operand rhs)
  | Lea { dest; src; size } ->
    sprintf "\tlea%s\t%s, %s" (format_size size) (format_mem src) (format_operand dest)
  | Lbl l -> Label.name l ^ ":"
  | Jump { op; label } -> sprintf "\t%s\t%s" (format_jump op) (Label.name label)
  | Set { op; src } -> sprintf "\t%s\t%s" (format_set op) (format_operand src)
  | Call fname -> sprintf "\tcall\t%s" fname
;;

let format_list l = List.map ~f:format l |> String.concat ~sep:"\n"

let pure_to_opr = function
  | AS.Add -> Add
  | AS.Sub -> Sub
  | AS.Mul -> Mul
  | AS.BitAnd -> And
  | AS.BitOr -> Or
  | AS.BitXor -> Xor
;;

let efkt_to_opr = function
  | AS.Div -> IDiv
  | AS.ShiftL -> Sal
  | AS.ShiftR -> Sar
  | AS.Mod -> Mod
;;

let unary_to_opr = function
  | AS.BitNot -> Not
;;

let callee_saved oper =
  match oper with
  | Reg r -> R.callee_saved r
  | _ -> raise (Failure "callee_saved can be applied only on Reg reg")
;;

let caller_saved oper =
  match oper with
  | Reg r -> R.caller_saved r
  | _ -> raise (Failure "caller_saved can be applied only on Reg reg")
;;

let all_available_regs =
  [
  (* AS.EAX *)
    (* AS.EDX *)
    (* AS.EDI
  ; AS.ESI (* ; AS.ECX removed for now because of shift operators *)
  *)
    (* AS.R8D
  ; AS.R9D
  ; AS.R12D
  ; AS.R13D
  ; AS.R14D
  ; AS.R15D *)
  ]
;;

let is_reg = function
  | Reg _ -> true
  | _ -> false
;;

let get_free (sz : size) : operand =
  Reg
    { reg = R.R11D
    ; size =
        (match sz with
        | L -> 4
        | Q -> 8)
    }
;;

let get_memfree (sz : size) : operand =
  Reg
    { reg = R.R10D
    ; size =
        (match sz with
        | L -> 4
        | Q -> 8)
    }
;;
