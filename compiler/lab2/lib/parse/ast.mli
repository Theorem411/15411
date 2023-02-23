(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *
 * Forward compatible fragment of C0
 *)

open Core
module T = Ctype

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

type unop =
  | Negative
  | L_not
  | B_not
[@@deriving sexp]

(* Notice that the subexpressions of an expression are marked.
 * (That is, the subexpressions are of type exp Mark.t, not just
 * type exp.) This means that source code location (a src_span) is
 * associated with the subexpression. Currently, the typechecker uses
 * this source location to print more helpful error messages.
 *
 * It's the parser and lexer's job to associate src_span locations with each
 * ast. It's instructive, but not necessary, to closely read the source code
 * for c0_parser.mly, c0_lexer.mll, and parse.ml to get a good idea of how
 * src_spans are created.
 *
 * Check out the Mark module for ways of converting a marked expression into
 * the original expression or to the src_span location. You could also just
 * look at how the typechecker gets the src_span from an expression when it
 * prints error messages.
 *
 * It's completely possible to remove Marks entirely from your compiler, but
 * it will be harder to figure out typechecking errors for later labs.
 *)
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
  | Call of
      { name : Symbol.t
      ; args : mexp list
      }

and mexp = exp Mark.t

type decl =
  | New_var of Symbol.t * T.t
  | Init of Symbol.t * T.t * mexp

type stm =
  | Declare of decl
  | Assign of
      { left : mexp
      ; right : mexp
      ; asgnop : binop option
      }
  | PostOp of
      { left : mexp
      ; op : binop
      }
  | Return of mexp option
  | Exp of mexp
  | If of
      { cond : mexp
      ; thenstm : mstm
      ; elsestm : mstm option
      }
  | For of
      { init : mstm option
      ; cond : mexp
      ; post : mstm option
      ; body : mstm
      }
  | While of
      { cond : mexp
      ; body : mstm
      }
  | Block of mstm list
  | Assert of mexp
  | Nop

and mstm = stm Mark.t

type function_body = mstm list

type param =
  | Param of
      { t : T.t
      ; name : Symbol.t
      }

type gdecl =
  | Typedef of
      { old_name : T.t
      ; new_name : T.t
      }
  | FunDec of
      { name : Symbol.t
      ; ret_type : T.t option
      ; params : param list
      }
  | FunDef of
      { name : Symbol.t
      ; ret_type : T.t option
      ; params : param list
      ; body : stm (* Block of mstm list *)
      }

type program = gdecl list

(* Print as source, with redundant parentheses *)
module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_stms : function_body -> string
  val pp_program : program -> string
end
