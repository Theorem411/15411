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

and mexp = exp Mark.t

type decl =
  | New_var of Symbol.t * _type
  | Init of Symbol.t * _type * mexp

type stm =
  | Declare of decl
  | Assign of mexp * mexp * binop option
  | PostOp of mexp * binop
  | Return of mexp
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
  | Nop
  | Label of string (*_ I am not sure about this*)
  | Goto of string (*_ I am not sure about this*)

and mstm = stm Mark.t

type program = mstm list

module Print = struct
  let pp_binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divided_by -> "/"
    | Modulo -> "%"
    | Less -> "<"
    | Less_eq -> "=<"
    | Greater -> ">"
    | Greater_eq -> ">="
    | Equals -> "=="
    | Not_equals -> "!="
    | L_and -> "&&"
    | L_or -> "||"
    | B_and -> "&"
    | B_xor -> "^"
    | B_or -> "|"
    | ShiftL -> "<<"
    | ShiftR -> ">>"
  ;;

  let pp_unop = function
    | Negative -> "-"
    | L_not -> "!"
    | B_not -> "~"
  ;;

  let rec pp_exp = function
    | Var id -> Symbol.name id
    | Const c -> Int32.to_string c
    | Unop unop -> sprintf "%s(%s)" (pp_unop unop.op) (pp_mexp unop.operand)
    | Binop binop ->
      sprintf "(%s %s %s)" (pp_mexp binop.lhs) (pp_binop binop.op) (pp_mexp binop.rhs)
    | True -> "true"
    | False -> "false"
    | Ternary t ->
      sprintf "(%s ? %s : %s)" (pp_mexp t.cond) (pp_mexp t.first) (pp_mexp t.second)

  and pp_mexp e = pp_exp (Mark.data e)

  let str_of__type = function
    | Integer -> "int"
    | Bool -> "bool"
  ;;

  let pp_decl = function
    | New_var (id, tp) -> sprintf "%s %s;" (str_of__type tp) (Symbol.name id)
    | Init (id, tp, e) ->
      sprintf "%s %s = %s;" (str_of__type tp) (Symbol.name id) (pp_mexp e)
  ;;

  let rec pp_stm = function
    | Declare d -> pp_decl d
    (* | Assign (id, e) -> sprintf "%s = %s;" (Symbol.name id) (pp_mexp e) *)
    | Return e -> sprintf "return %s;" (pp_mexp e)
    | Nop -> "nop;"
    | Assign (lhs, rhs, None) -> sprintf "%s = %s;" (pp_mexp lhs) (pp_mexp rhs)
    | Assign (lhs, rhs, Some op) ->
      sprintf "%s %s= %s;" (pp_mexp lhs) (pp_binop op) (pp_mexp rhs)
    | PostOp (e, op) -> sprintf "%s%s%s" (pp_mexp e) (pp_binop op) (pp_binop op)
    | Exp e -> sprintf "%s" (pp_mexp e)
    | If { elsestm = None; thenstm = t; cond = e } ->
      sprintf "if (%s) [\n%s;]" (pp_mexp e) (pp_mstm t)
    | If { elsestm = Some s; thenstm = t; cond = e } ->
      sprintf "if (%s) \nthen [\n%s]\nELSE[\n%s]" (pp_mexp e) (pp_mstm t) (pp_mstm s)
    | Label l -> "." ^ l
    | Goto l -> "GOTO " ^ l
    | Block stms -> sprintf "{ %s }" (pp_stms stms)
    | While { cond = c; body = b } -> sprintf "WHILE (%s) %s" (pp_mexp c) (pp_mstm b)
    | For f ->
      sprintf
        "For(%s , %s , %s) %s"
        (pp_mstm_opt f.init)
        (pp_mexp f.cond)
        (pp_mstm_opt f.post)
        (pp_mstm f.body)

  and pp_mstm stm = pp_stm (Mark.data stm)
  and pp_stms stms = String.concat (List.map ~f:(fun stm -> pp_mstm stm ^ "\n") stms)

  and pp_mstm_opt = function
    | None -> ""
    | Some m -> pp_mstm m
  ;;

  let pp_program stms = "{\n" ^ pp_stms stms ^ "}"
end
