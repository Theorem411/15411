module Typ = Ctype

type binop_pure = 
  | Plus
  | Minus
  | Times
  | And
  | Or
  | Leq 
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type binop_efkt = 
  | Divided_by
  | Modulo

type unop = 
  | Not

(* type boolean = T | F *)
(*_ all subclasses of exp type  *)
type exp = 
  | True
  | False
  | Var of Symbol.t
  | Const of Int32.t
  | PureBinop of  
      { op : binop_pure
      ; lhs : mexp
      ; rhs : mexp
      }
  | EfktBinop of 
      { op : binop_efkt
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of 
      { op : unop
      ; operand : mexp;
      }
and mexp = exp Mark.t

(*_ all subclasses of stmt type and the program type *)
type stmt = 
  | Declare of 
      { var : Symbol.t
      ; typ : Typ.t
      ; body : program
      }
  | Assign of 
      { var : Symbol.t
      ; exp : mexp
      }
  | If of 
      { cond : mexp
      ; lb : program
      ; rb : program}
  | While of 
      { cond : mexp
      ; body : program
      }
  | Return of mexp
  | Nop
  | Seq of program * program
  | NakedExpr of mexp
and program = stmt Mark.t



