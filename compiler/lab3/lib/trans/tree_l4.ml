open Core
module A = Asts

type pbop =
  | Add
  | Sub
  | Mul
  | BitAnd
  | BitOr
  | BitXor

type ebop =
  | Div
  | Mod
  | ShftL
  | ShftR

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
  | PtrAddr of ptraddr
  | ArrAddr of arraddr
  | Mem of addr

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
      { dest : Temp.t option
      ; fname : Symbol.t
      ; args : mpexp list
      }
  | MovToMemPure of
      { mem : mpexp
      ; src : mpexp
      }
  | MovToMemEfkt of
      { mem : mpexp
      ; ebop : ebop
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | MovToMemFunc of
      { mem : mpexp
      ; fname : Symbol.t
      ; args : mpexp list
      }
  | Return of mpexp option
  | AssertFail

type jump_t =
  | Ret
  | Uncon of Label.t
  | Cond of
      { lt : Label.t
      ; lf : Label.t
      }

type block =
  { label : Label.t
  ; block : stm list
  ; jump : jump_t
  }

type fspace_block =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef : block list
  }

type program = fspace_block list

module Print = struct
  let pp_pexp (e : pexp) = failwith "no"
  let pp_mpexp ((e, _) : mpexp) = pp_pexp e
  let pp_stm (stm : stm) = failwith "no"
  let pp_program (prog : program) = failwith "no"
end