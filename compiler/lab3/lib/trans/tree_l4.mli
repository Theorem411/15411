module A = Asts

type pbop =
  | Add
  | Sub
  | Mul
  | BitAnd
  | BitOr
  | BitXor
[@@deriving equal]

type ebop =
  | Div
  | Mod
  | ShftL
  | ShftR
[@@deriving equal]

type ibop =
  | Pure of pbop
  | Efkt of ebop

type cbop =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type unop = BitNot

(*_ pure expression: binary operator can only be pure operator *)
type pexp =
  | Const of Int32.t
  | Temp of Temp.t
  | Binop of
      { op : pbop
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | Cmpop of
      { op : cbop
      ; size : int
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | Unop of
      { op : unop
      ; p : mpexp
      }
  | Mem of addr (*_ deref of address *)
  | Addr of addr (*_ just the raw address *)

and ptraddr =
  { start : mpexp
  ; off : int
  }

and arraddr =
  { head : mpexp
  ; idx : mpexp
  ; typ_size : int
  ; extra : int
  }

and addr =
  | Ptr of ptraddr
  | Null
  | Arr of arraddr

and mpexp = pexp * int

type cond =
  | LCond of
      { cmp : cbop
      ; p1 : mpexp
      ; p2 : mpexp
      }
  | SCond of
      { cmp : cbop
      ; p1 : mpexp
      ; p2 : mpexp
      }

type stm =
  | If of
      { cond : cond
      ; lt : Label.t
      ; lf : Label.t
      }
  | Goto of Label.t
  | Label of Label.t
  | MovEfktExp of
      { dest : Temp.t
      ; ebop : ebop
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | MovPureExp of
      { dest : Temp.t
      ; src : mpexp
      }
  | MovFuncApp of
      { dest : (Temp.t * int) option
      ; fname : Symbol.t
      ; args : mpexp list
      ; tail_call : bool
      }
  | Alloc of
      { dest : Temp.t
      ; size : int
      }
  | Calloc of
      { dest : Temp.t
      ; len : mpexp
      ; typ : int
      }
  | MovToMem of
      { (*_ this means deref the lhs *)
        addr : addr
      ; src : mpexp
      }
  | Return of mpexp option
  | AssertFail

type jump_t =
  | JRet
  | JCon of
      { lt : Label.t
      ; lf : Label.t
      }
  | JUncon of Label.t

type block =
  { label : Label.bt
  ; block : stm list
  ; jump : jump_t
  ; loop_depth : int
  ; is_empty : bool
  }

type fspace_block =
  { fname : Symbol.t
  ; args : (Temp.t * int) list
  ; fdef : block list
  ; ret_size : int option
  }

type program = fspace_block list

val size : mpexp -> int

module Print : sig
  val pp_pexp : pexp -> string
  val pp_mpexp : mpexp -> string
  val pp_stm : stm -> string
  val pp_block : block -> string
  val pp_fspace : fspace_block -> string
  val pp_program : program -> string
end