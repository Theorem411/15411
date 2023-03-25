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
  | RCX
  | RDX
  | RSI
  | RDI
  | RBX
[@@deriving equal, sexp, compare, enum, hash]

type reg =
  { reg : reg_enum
  ; size : int
  }
[@@deriving equal, sexp, compare, hash]

val format_reg : reg -> string
val callee_saved: reg -> bool
val caller_saved: reg -> bool