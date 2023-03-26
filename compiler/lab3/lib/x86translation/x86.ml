open Core
module AS = Assem_l4
module R = Register


let as_to_reg_enum: AS.reg -> R.reg_enum = function
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

type operand =
  | Imm of Int32.t
  | Stack of int
  | Reg of R.reg
  | Mem of
      { base_reg : R.reg
      ; idx_reg : R.reg option
      ; scale : int option
      ; disp : int option
      }
[@@deriving equal, compare, sexp]

(* Stack n means that the varialbe is in -n(%rbp) *)
let format_operand = function
  | Imm n -> "$" ^ Int32.to_string n
  | Reg r -> R.format_reg r
  | Stack n -> string_of_int n ^ "(%rsp)"
  | Mem { disp; base_reg; idx_reg; scale } ->
    (match idx_reg, scale, disp with
    | Some idx, Some sc, Some disp ->
      sprintf "[%s + %d*%s + %d]" (R.format_reg base_reg) sc (R.format_reg idx) disp
    | Some idx, Some sc, None ->
      sprintf "[%s + %d*%s]" (R.format_reg base_reg) sc (R.format_reg idx)
    | Some _, None, _ -> failwith "no scale when idx is at memory"
    | None, Some _, _ -> failwith "no index register when given scale"
    | None, None, Some disp -> sprintf "[%s + %d]" (R.format_reg base_reg) disp
    | None, None, None -> sprintf "[%s]" (R.format_reg base_reg))
;;

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
[@@deriving equal, compare, sexp]

let format_operation = function
  | Add -> "addl"
  | Sub -> "subl"
  | Addq -> "addq"
  | Subq -> "subq"
  | Mul -> "imull"
  | IDiv -> "idivl"
  | Mov -> "mov"
  | Cltd -> "cltd"
  | Cqde -> "cdqe"
  | Pushq -> "pushq"
  | Popq -> "popq"
  | Movq -> "movq"
  | IMul -> "lol"
  | Movl -> "movl"
  | Movzx -> "movzx"
  | Movsx -> "movsx"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "not"
  | Cmp -> "cmp"
  | Mod -> "mod"
  | Sal -> "sal"
  | Sar -> "sar"
  | Call -> "call"
  | Test -> "test"
  | Div -> "div"
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
  | ZeroCommand of { op : operation }
  | Directive of string
  | Cmp of
      { rhs : operand
      ; lhs : operand
      }
  | Test of
      { rhs : operand
      ; lhs : operand
      }
  | Lea of
      { dest : operand
      ; src : operand
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
[@@deriving equal, compare, sexp]

let format = function
  | BinCommand { op = (Addq | Subq) as bop; src = s; dest = d } ->
    sprintf "\t%s\t%s, %s" (format_operation bop) (format_operand s) (format_operand d)
  | BinCommand { op = (Sal | Sar | Movzx) as bop; src = s; dest = d } ->
    sprintf "\t%s\t%s, %s" (format_operation bop) (format_operand s) (format_operand d)
  | BinCommand binop ->
    sprintf
      "\t%s\t%s, %s"
      (format_operation binop.op)
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
  | Cmp { rhs; lhs } -> sprintf "\tcmp\t%s, %s" (format_operand lhs) (format_operand rhs)
  | Test { rhs; lhs } ->
    sprintf "\ttest\t%s, %s" (format_operand lhs) (format_operand rhs)
    | Lea { dest; src } ->
      sprintf "\ttest\t%s, %s" (format_operand dest) (format_operand src)
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
  [ (* AS.EAX *)
    (* AS.EDX *)
    (* AS.EDI
  ; AS.ESI (* ; AS.ECX removed for now because of shift operators *)
  ; AS.R8D
  ; AS.R9D
  ; AS.R10D
  ; AS.EBX *)
    (* AS.R12D
  ; AS.R13D
  ; AS.R14D
  ; AS.R15D *) ]
;;

let is_reg = function
  | Reg _ -> true
  | _ -> false
;;

let __FREE_REG (sz : int) : operand = Reg { reg = R.R11D; size = sz }
