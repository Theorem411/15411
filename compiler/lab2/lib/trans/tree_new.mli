(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)
module A = Aste

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

type cbop =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq
  
type unop = 
  | BitNot
(*_ pure expression: binary operator can only be pure operator *)
type pexp =
  | Const of Int32.t
  | Temp of Temp.t
  | Binop of
      { op : pbop
      ; lhs : pexp
      ; rhs : pexp
      }
  | Cmpop of 
      { op : cbop
      ; lhs : pexp
      ; rhs : pexp
      }
  | Unop of 
      { op : unop
      ; p : pexp
      }
and cond = 
  { cmp : cbop
  ; p1 : pexp
  ; p2 : pexp
  }
and stm =
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
      ; lhs : pexp
      ; rhs : pexp
      }
  | MovPureExp of
      { dest : Temp.t
      ; src : pexp
      }
  | Return of pexp

type program = stm list

module Print : sig
  val pp_pexp : pexp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
