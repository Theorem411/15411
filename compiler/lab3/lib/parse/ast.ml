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
module T = Ctype_l4

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
  | Null
  | Deref of mexp
  | ArrAccess of
      { arr : mexp
      ; idx : mexp
      }
  | StructDot of
      { str : mexp
      ; field : Symbol.t
      }
  | StructArr of
      { str : mexp
      ; field : Symbol.t
      }
  | Alloc of T.tau
  | Alloc_array of
      { typ : T.tau
      ; len : mexp
      }

and mexp = exp Mark.t

type decl =
  | New_var of Symbol.t * T.tau
  | Init of Symbol.t * T.tau * mexp

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
      { t : T.tau
      ; name : Symbol.t
      }

type gdecl =
  | Typedef of
      { old_name : T.tau
      ; new_name : T.tau
      }
  | FunDec of
      { name : Symbol.t
      ; ret_type : T.tau option
      ; params : param list
      }
  | FunDef of
      { name : Symbol.t
      ; ret_type : T.tau option
      ; params : param list
      ; body : mstm (* Block of mstm list *)
      }
  | Sdecl of Symbol.t
  | Sdef of
      { sname : Symbol.t
      ; ssig : (Symbol.t * T.tau) list
      }

and mgdecl = gdecl Mark.t

type program = mgdecl list

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
    | Call { name; args } ->
      sprintf
        "%s(%s)"
        (Symbol.name name)
        (String.concat ~sep:"," (List.map args ~f:pp_mexp))
    | Ternary t ->
      sprintf "(%s ? %s : %s)" (pp_mexp t.cond) (pp_mexp t.first) (pp_mexp t.second)
    | Null -> "null"
    | Deref p -> sprintf "*%s" (pp_mexp p)
    | ArrAccess { arr; idx } -> sprintf "%s[%s]" (pp_mexp arr) (pp_mexp idx)
    | StructDot { str; field } -> sprintf "%s.%s" (pp_mexp str) (Symbol.name field)
    | StructArr { str; field } -> sprintf "%s->%s" (pp_mexp str) (Symbol.name field)
    | Alloc tau -> sprintf "alloc(%s)" (T._tau_tostring tau)
    | Alloc_array { typ; len } ->
      sprintf "calloc(%s, %s)" (T._tau_tostring typ) (pp_mexp len)

  and pp_mexp e = pp_exp (Mark.data e)

  let pp_decl = function
    | New_var (id, tp) -> sprintf "%s %s;" (T._tau_tostring tp) (Symbol.name id)
    | Init (id, tp, e) ->
      sprintf "%s %s = %s;" (T._tau_tostring tp) (Symbol.name id) (pp_mexp e)
  ;;

  let pp_param = function
    | Param { t; name } -> sprintf "%s %s" (T._tau_tostring t) (Symbol.name name)
  ;;

  let pp_params ps = String.concat ~sep:"," (List.map ps ~f:pp_param)

  let rec pp_stm = function
    | Declare d -> pp_decl d
    (* | Assign (id, e) -> sprintf "%s = %s;" (Symbol.name id) (pp_mexp e) *)
    | Return e ->
      sprintf
        "return %s;"
        (match e with
        | None -> ""
        | Some x -> pp_mexp x)
    | Nop -> "nop;"
    | Assign { left = lhs; right = rhs; asgnop = None } ->
      sprintf "%s = %s;" (pp_mexp lhs) (pp_mexp rhs)
    | Assign { left = lhs; right = rhs; asgnop = Some op } ->
      sprintf "%s %s= %s;" (pp_mexp lhs) (pp_binop op) (pp_mexp rhs)
    | PostOp { left = e; op } -> sprintf "%s%s%s" (pp_mexp e) (pp_binop op) (pp_binop op)
    | Exp e -> sprintf "%s" (pp_mexp e)
    | If { elsestm = None; thenstm = t; cond = e } ->
      sprintf "if (%s) [\n%s;]" (pp_mexp e) (pp_mstm t)
    | If { elsestm = Some s; thenstm = t; cond = e } ->
      sprintf "if (%s) \nthen [\n%s]\nELSE[\n%s]" (pp_mexp e) (pp_mstm t) (pp_mstm s)
    | Block stms -> sprintf "{\n %s }\n" (pp_stms stms)
    | While { cond = c; body = b } -> sprintf "WHILE (%s) %s" (pp_mexp c) (pp_mstm b)
    | For f ->
      sprintf
        "For(%s , %s , %s) %s"
        (pp_mstm_opt f.init)
        (pp_mexp f.cond)
        (pp_mstm_opt f.post)
        (pp_mstm f.body)
    | Assert e -> sprintf "Assert(%s)" (pp_mexp e)

  and pp_mstm stm = pp_stm (Mark.data stm)
  and pp_stms stms = String.concat (List.map ~f:(fun stm -> pp_mstm stm ^ "\n") stms)

  and pp_mstm_opt = function
    | None -> ""
    | Some m -> pp_mstm m

  and pp_fundec name ret_type params =
    let rt =
      match ret_type with
      | None -> "void"
      | Some x -> T._tau_tostring x
    in
    sprintf "%s %s (%s)" rt (Symbol.name name) (pp_params params)

  and pp_gdecl_single = function
    | Typedef { old_name : T.tau; new_name : T.tau } ->
      sprintf "typedef %s <--- %s;" (T._tau_tostring new_name) (T._tau_tostring old_name)
    | FunDec { name : Symbol.t; ret_type : T.tau option; params : param list } ->
      pp_fundec name ret_type params ^ ";"
    | FunDef
        { name : Symbol.t
        ; ret_type : T.tau option
        ; params : param list
        ; body : mstm (* Block of mstm list *)
        } -> sprintf "%s %s;" (pp_fundec name ret_type params) (pp_mstm body)
    | Sdecl s -> sprintf "declare struct %s " (Symbol.name s)
    | Sdef { sname; ssig } ->
      sprintf
        "struct %s [%s]"
        (Symbol.name sname)
        (String.concat
           ~sep:","
           (List.map ssig ~f:(fun (name, t) ->
                sprintf "%s %s" (T._tau_tostring t) (Symbol.name name))))

  (* and pp_gdecl gdecls = String.concat ~sep:"\n" (List.map gdecls ~f:pp_gdecl_single) *)
  and pp_mgdecl gd = pp_gdecl_single (Mark.data gd)
  and pp_program p = String.concat (List.map ~f:(fun mgd -> pp_mgdecl mgd ^ "\n") p)
end
