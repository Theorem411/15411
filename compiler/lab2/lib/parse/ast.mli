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
  | Shift_left
  | Shift_right

type unop = Negative | L_not | B_not

type _type = Bool | Integer 

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
      ; tstm : mstm
      ; fstm : mstm option
      }
  | For of
      { init : mexp
      ; cond : mexp
      ; post : mexp
      ; body : mstm
      }
  | ForDef of
      { init : decl
      ; cond : mexp
      ; post : mexp
      ; body : mstm
      }
  | While of
      { init : decl
      ; body : mstm
      }
  | Block of block
  | Nop

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
