(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)
module A = Aste

type int_pbop =
  | Add
  | Sub
  | Mul
  | BitAnd
  | BitOr
  | BitXor

type log_pbop =
  | And
  | Or
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type pbop =
  | IntOp of int_pbop
  | LogOp of log_pbop

val aste_pure_binop_to_pbop : A.binop_pure -> pbop

type int_ebop =
  | Div
  | Mod
  | ShftL
  | ShftR

type ebop = IntOp of int_ebop

val aste_efkt_binop_to_ebop : A.binop_efkt -> ebop

(*_ pure expression: binary operator can only be pure operator *)
type pexp =
  | Const of Int32.t
  | Temp of Temp.t
  | Pbop of
      { op : pbop
      ; lhs : pexp
      ; rhs : pexp
      }

and stm =
  | If of
      { cond : pexp (*_ cond should only be typed-checked to be boolean-only *)
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
