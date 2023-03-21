open Core
type intop_pure =
  | Plus
  | Minus
  | Times
  | BitAnd
  | BitOr
  | BitXor

type intop_cmp =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type intop_efkt =
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
  | Var of
      { var : Symbol.t
      ; type_size : int
      }
  | Const of Int32.t
  | Ternary of
      { cond : mexp
      ; lb : mexp
      ; rb : mexp
      }
  | PureBinop of
      { op : intop_pure
      ; lhs : mexp
      ; rhs : mexp
      }
  | EfktBinop of
      { op : intop_efkt
      ; lhs : mexp
      ; rhs : mexp
      }
  | CmpBinop of
      { op : intop_cmp
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
  | Deref of address
  | ArrayAccess of address
  | StructAccess of address
  | Alloc of int
  | Alloc_array of
      { type_size : int
      ; len : mexp
      }

and mexp = exp * int

and address =
  { ptr : mexp
  ; off : int
  }

type stm =
  | Declare of
      { var : Symbol.t
      ; assign : mexp option
      ; body : mstm
      }
  | AssignToSymbol of
      { var : Symbol.t
      ; exp : mexp
      }
  | AsopMemPure of
      { addr : address
      ; op : intop_pure option
      ; exp : mexp
      }
  | AsopMemEfkt of
      { addr : address
      ; op : intop_efkt option
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

and mstm = stm * int

type glob =
  { f : Symbol.t
  ; args : (Symbol.t * int) list
  ; fdef : mstm
  }

type mglob = glob Mark.t
type program = mglob list

module Print : sig
  val pp_exp : exp -> string
  val pp_mexp : mexp -> string
  val pp_stm : stm -> string
  val pp_mstm : mstm -> string
  val pp_glob : glob -> string
  val pp_mglob : mglob -> string
  val print_all : program -> string
end
