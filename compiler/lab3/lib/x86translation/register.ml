open Core

type reg_enum =
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

type reg =
  { reg : reg_enum
  ; size : int
  }
[@@deriving equal, sexp, compare, hash]

let eax = {reg = EAX; size = 4}

let arg_i_to_reg sz = function
  | 0 -> { reg = EDI; size = sz }
  | 2 -> { reg = EDX; size = sz }
  | 1 -> { reg = ESI; size = sz }
  | 3 -> { reg = ECX; size = sz }
  | 4 -> { reg = R8D; size = sz }
  | 5 -> { reg = R9D; size = sz }
  | _ -> failwith "args overflow 6"
;;

(* fixig equal bugs *)
let equal_reg a b =
  match a, b with
  | { reg = rega; _ }, { reg = regb; _ } -> equal_reg_enum rega regb
;;

let format_reg_32 = function
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
  | RBP -> "%ebp"
  | RSP -> "%esp"
;;

let format_reg_64 = function
  | EAX -> "%rax"
  | EBX -> "%rbx"
  | ECX -> "%rcx"
  | EDX -> "%rdx"
  | ESI -> "%rsi"
  | EDI -> "%rdi"
  | R8D -> "%r8"
  | R9D -> "%r9"
  | R10D -> "%r10"
  | R11D -> "%r11"
  | R12D -> "%r12"
  | R13D -> "%r13"
  | R14D -> "%r14"
  | R15D -> "%r15"
  | RBP -> "%rbp"
  | RSP -> "%rsp"
;;

let format_reg_16 = function
  | EAX -> "%al"
  | ECX -> "%cl"
  | r -> failwith ("word(16) not supported yet for " ^ format_reg_32 r)
;;

let format_reg = function
  | { reg; size } ->
    (match size with
    | 2 -> format_reg_16 reg
    | 4 -> format_reg_32 reg
    | 8 -> format_reg_64 reg
    | _ -> failwith (sprintf "reg[%s] can not have size %d" (format_reg_32 reg) size))
;;

let callee_saved = function
  | { reg; _ } ->
    (match reg with
    | EBX | RSP | RBP | R12D | R13D | R14D | R15D -> true
    | _ -> false)
;;

let caller_saved = function
  | { reg; _ } ->
    (match reg with
    | EAX | EDI | ESI | EDX | ECX | R8D | R9D | R10D | R11D -> true
    | _ -> false)
;;