open Core
module Typ = Ctype

type binop_pure =
  | Plus
  | Minus
  | Times
  | BitAnd
  | BitOr
  | BitXor

type binop_cmp =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type binop_efkt =
  | Divided_by
  | Modulo
  | ShiftL
  | ShiftR

type unop =
  | BitNot (*bitwise not*)
  | LogNot (*!*)

(*_ all subclasses of exp type  *)
type exp =
  | True
  | False
  | Var of Symbol.t
  | Const of Int32.t
  | Ternary of
      { cond : mexp
      ; lb : mexp
      ; rb : mexp
      }
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
  | CmpBinop of
      { op : binop_cmp
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of
      { op : unop
      ; operand : mexp
      }

and mexp = exp Mark.t

type stmt =
  | Declare of
      { var : Symbol.t
      ; typ : Typ.t
      ; body : mstmt
      }
  | Assign of
      { var : Symbol.t
      ; exp : mexp
      }
  | If of
      { cond : mexp
      ; lb : mstmt
      ; rb : mstmt
      }
  | While of
      { cond : mexp
      ; body : mstmt
      }
  | Return of mexp
  | Nop
  | Seq of mstmt * mstmt
  | NakedExpr of mexp

and mstmt = stmt Mark.t

type global = 
  | Typedef of Typ.t * Typ.t
  | Fundecl of 

module Print : sig
  val pp_exp : exp -> string
  val pp_stm : ?n:int -> mstmt -> string
  val pp_program : ?n:int -> mstmt -> string
  val print_all : mstmt -> string
end
