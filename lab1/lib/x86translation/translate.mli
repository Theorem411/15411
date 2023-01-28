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
  | R12D
  | R13D
  | R14D
  | R15D
[@@deriving equal]

type operand =
  | Imm of Int32.t
  | Reg of reg
  | Temp of Temp.t
[@@deriving equal]

type operation =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Mov

type instr =
  | Binop of
      { op : operation
      ; dest : operand
      ; src : operand
      }
  | Directive of string
  | Comment of string
