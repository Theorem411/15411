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

val allregs : reg_enum list
val format_reg_32 : reg_enum -> string
val format_reg : reg -> string
val callee_saved: reg_enum -> bool
val caller_saved: reg_enum -> bool
val arg_i_to_reg: int -> int -> reg

val eax: reg