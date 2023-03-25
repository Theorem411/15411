open Core
module A = Aste_l4

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

type ptrop_cmp =
  | PtrEq
  | PtrNeq

type intop_efkt =
  | Divided_by
  | Modulo
  | ShiftL
  | ShiftR

type intop =
  | Pure of intop_pure
  | Efkt of intop_efkt

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
      { cond : exp
      ; lb : exp
      ; rb : exp
      }
  | PureBinop of
      { op : intop_pure
      ; lhs : exp
      ; rhs : exp
      }
  | EfktBinop of
      { op : intop_efkt
      ; lhs : exp
      ; rhs : exp
      }
  | CmpBinop of
      { op : intop_cmp
      ; lhs : exp
      ; rhs : exp
      }
  | CmpPointer of
      { op : ptrop_cmp
      ; lhs : ptraddr
      ; rhs : ptraddr
      }
  | Unop of
      { op : unop
      ; operand : exp
      }
  | Call of
      { name : Symbol.t
      ; args : (exp * int) list
      }
  | Deref of ptraddr
  | ArrayAccess of arraddr
  | StructAccess of ptraddr
  | Alloc of int
  | Alloc_array of
      { type_size : int
      ; len : exp
      }
  | PtrAddr of ptraddr
  | ArrAddr of arraddr

and arraddr =
  { head : exp
  ; idx : exp
  ; size : int
  ; extra : int
  }

and ptraddr =
  | Ptr of
      { start : exp
      ; off : int
      }
  | Null

type stm =
  | Declare of
      { var : Symbol.t
      ; size : int
      ; assign : exp option
      ; body : mstm
      }
  | Assign of
      { var : Symbol.t
      ; size : int
      ; exp : exp
      }
  | Asop of
      { dest : exp
      ; size : int
      ; op : intop option
      ; exp : exp
      }
  | If of
      { cond : exp
      ; lb : mstm
      ; rb : mstm
      }
  | While of
      { cond : exp
      ; body : mstm
      }
  | Return of (exp * int) option
  | Nop
  | Seq of mstm * mstm
  | NakedExpr of (exp * int)
  | AssertFail
  | NakedCall of
      { name : Symbol.t
      ; args : (exp * int) list
      }

and mstm = stm Mark.t

type glob =
  { f : Symbol.t
  ; args : (Symbol.t * int) list
  ; fdef : mstm
  }

type mglob = glob Mark.t
type program = mglob list

val intop_pure : A.binop_pure -> intop_pure
val intop_efkt : A.binop_efkt -> intop_efkt
val intop_cmp : A.binop_cmp -> intop_cmp
val ptrop_cmp : A.binop_cmp -> ptrop_cmp
val unop : A.unop -> unop
val ptraddr_exn : exp -> ptraddr
val arraddr_exn : exp -> arraddr

module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_mstm : mstm -> string
  val pp_glob : glob -> string
  val pp_mglob : mglob -> string
  val print_all : program -> string
end
