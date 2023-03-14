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
  | Call of
      { name : Symbol.t
      ; args : mexp list
      }
  | Null
  | Deref of mexp
  | ArrAccess of
      { arr : mexp
      ; idx : mexp
      }
  | StructDot of
      { str : mexp
      ; field : Symbol.t
      }
  | StructArr of
      { str : mexp
      ; field : Symbol.t
      }
  | Alloc of Typ.tau
  | Alloc_array of
      { typ : Typ.tau
      ; len : mexp
      }

and mexp = exp Mark.t

type stm =
  | Declare of
      { var : Symbol.t
      ; typ : Typ.tau
      ; assign : mexp option
      ; body : mstm
      }
  | Assign of
      { dest : mexp
      ; exp : mexp
      }
  | If of
      { cond : mexp
      ; lb : mstm
      ; rb : mstm
      }
  | While of
      { cond : mexp
      ; body : mstm
      }
  | Return of mexp option
  | Nop
  | Seq of mstm * mstm
  | NakedExpr of mexp
  | AssertFail
  | NakedCall of
      { name : Symbol.t
      ; args : mexp list
      }

and mstm = stm Mark.t

type glob =
  | Typedef of
      { told : Typ.tau
      ; tnew : Typ.tau
      }
  | Fundecl of
      { f : Symbol.t
      ; args : Symbol.t list
      ; fsig : Typ.fsig
      }
  | Fundef of
      { f : Symbol.t
      ; args : Symbol.t list
      ; fsig : Typ.fsig
      ; fdef : mstm
      }
  | Sdecl of Symbol.t
  | Sdef of
      { sname : Symbol.t
      ; ssig : (Symbol.t * Typ.tau) list
      }

type mglob = glob Mark.t
type program = mglob list

module Print : sig
  val pp_exp : exp -> string
  val pp_mexp : mexp -> string
  val pp_stm : ?n:int -> stm -> string
  val pp_mstm : ?n:int -> mstm -> string
  val pp_glob : ?n:int -> glob -> string
  val pp_mglob : ?n:int -> mglob -> string
  val print_all : program -> string
end
