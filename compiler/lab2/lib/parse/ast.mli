(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Forward compatible fragment of C0
 *)

(* Operator *)
type binop =
  | Plus
  | Minus
  | Times
  | Divided_by
  | Modulo
  | Less
  | Less_eq
  | Greater
  | Greater_eq
  | Equals
  | Not_equals
  | L_and
  | L_or
  | B_and
  | B_xor
  | B_or
  | ShiftL
  | ShiftR
[@@deriving sexp]

type _type =
  | Bool
  | Integer
[@@deriving sexp]

type unop =
  | Negative
  | L_not
  | B_not
[@@deriving sexp]

(* Expression *)
type exp =
  | Var of Symbol.t
  | Const of Int32.t
  | True
  | False
  | Binop of
      { op : binop
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of
      { op : unop
      ; operand : mexp
      }
  | Ternary of
      { cond : mexp
      ; first : mexp
      ; second : mexp
      }

(* Expression plus src file location *)
and mexp = exp Mark.t

(* Declaration *)
type decl =
  (* _type x; *)
  | New_var of Symbol.t * _type
  (* _type x = e; *)
  | Init of Symbol.t * _type * mexp

(* Statement *)
type stm =
  | Declare of decl
  | Assign of Symbol.t * mexp
  | Return of mexp
  | Exp of mexp
  | If of
      { cond : mexp
      ; thenstm : mstm
      ; elsestm : mstm option
      }
  | For of
      { init : mstm
      ; cond : mexp
      ; post : mexp
      ; body : mstm
      }
  | ForDef of
      { init : decl
      ; cond : mexp
      ; post : mstm
      ; body : mstm
      }
  | While of
      { cond : mexp
      ; body : mstm
      }
  | Block of block
  | Nop
  | Label of string (*_ I am not sure about this*)
  | Goto of string (*_ I am not sure about this*)

(* Statement plus src file location *)
and mstm = stm Mark.t

(* block *)
and block = mstm list

type program = mstm list

(* Print as source, with redundant parentheses *)
module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
