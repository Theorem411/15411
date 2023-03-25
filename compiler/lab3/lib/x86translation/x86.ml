open Core
module AS = Assem_l4
module R = Register

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
  | Mov
  | Movl
  | Movq
  | Movzx
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
  | Pushq -> "pushq"
  | Popq -> "popq"
  | Movq -> "movq"
  | IMul -> "lol"
  | Movl -> "movl"
  | Movzx -> "movzx"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "not"
  | Cmp -> "cmp"
  | Mod -> "mod"
  | Sal -> "sal"
  | Sar -> "sar"
  | Call -> "call"
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
    | AS.Jg -> "jg" (*_ jump if p1 > p2 *))
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
