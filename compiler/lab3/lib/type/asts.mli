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

(* type ptrop_cmp =
  | PtrEq
  | PtrNeq *)

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
  | Var of Symbol.t
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
      ; size : int
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
  | Deref of ptraddr
  | ArrayAccess of arraddr
  | StructAccess of ptraddr
  | Alloc of int
  | Alloc_array of
      { type_size : int
      ; len : mexp
      }
  | PtrAddr of ptraddr
  | ArrAddr of arraddr

and arraddr =
  { head : mexp
  ; idx : mexp
  ; size : int
  ; extra : int
  }

and ptraddr =
  | Ptr of
      { start : mexp
      ; off : int
      }
  | Null

and mexp = exp * int

type stm =
  | Declare of
      { var : Symbol.t
      ; assign : mexp option
      ; body : stm
      }
  | Assign of
      { var : Symbol.t
      ; exp : mexp
      }
  | AssignToPtrMem of
      { dest : Symbol.t
      ; op : intop option
      ; exp : mexp
      }
  | AssignToArrMem of
      { dest : Symbol.t
      ; op : intop option
      ; exp : mexp
      }
  | If of
      { cond : mexp
      ; lb : stm
      ; rb : stm
      }
  | While of
      { cond : mexp
      ; body : stm
      }
  | Return of mexp option
  | Nop
  | Seq of stm * stm
  | NakedExpr of mexp
  | AssertFail
  | NakedCall of
      { name : Symbol.t
      ; args : mexp list
      }

type glob =
  { f : Symbol.t
  ; args : (Symbol.t * int) list
  ; fdef : stm
  }

type program = glob list

val intop : A.binop -> intop
val intop_pure : A.binop_pure -> intop_pure
val intop_efkt : A.binop_efkt -> intop_efkt
val intop_cmp : A.binop_cmp -> intop_cmp
val ptrop_cmp : A.binop_cmp -> intop_cmp
val unop : A.unop -> unop
val ptraddr_exn : exp -> ptraddr
val arraddr_exn : exp -> arraddr
val size : mexp -> int

module Print : sig
  val pp_exp : exp -> string
  val pp_mexp : mexp -> string
  val pp_stm : stm -> string
  val pp_glob : glob -> string
  val print_all : program -> string
end
