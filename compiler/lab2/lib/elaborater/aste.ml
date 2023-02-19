open Core
module Typ = Ctype

type binop_pure =
  | Plus
  | Minus
  | Times
  | BitAnd
  | BitOr
  | BitXor

type binop_cmp =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type binop_efkt =
  | Divided_by
  | Modulo
  | ShiftL
  | ShiftR

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
      { op : binop_pure
      ; lhs : mexp
      ; rhs : mexp
      }
  | EfktBinop of
      { op : binop_efkt
      ; lhs : mexp
      ; rhs : mexp
      }
  | CmpBinop of
      { op : binop_cmp
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of
      { op : unop
      ; operand : mexp
      }

and mexp = exp Mark.t

type stmt =
  | Declare of
      { var : Symbol.t
      ; typ : Typ.t
      ; body : program
      }
  | Assign of
      { var : Symbol.t
      ; exp : mexp
      }
  | If of
      { cond : mexp
      ; lb : program
      ; rb : program
      }
  | While of
      { cond : mexp
      ; body : program
      }
  | Return of mexp
  | Nop
  | Seq of program * program
  | NakedExpr of mexp

and program = stmt Mark.t

module Print = struct
  let repeat s n =
    let rec helper s1 n1 = if n1 = 0 then s1 else helper (s1 ^ s) (n1 - 1) in
    helper "" n
  ;;

  let tabs = repeat "  "

  let pp_binop_pure = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | BitAnd -> "&"
    | BitOr -> "|"
    | BitXor -> "^"
  ;;

  let pp_binop_comp = function
    | Less -> "<"
    | Greater -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | Eq -> "=="
    | Neq -> "!="
  ;;

  let pp_binop_efkt = function
    | Divided_by -> "/"
    | Modulo -> "%"
    | ShiftL -> "<<"
    | ShiftR -> ">>"
  ;;

  let pp_unop = function
    | BitNot -> "~"
    | LogNot -> "!"
  ;;

  let rec pp_exp = function
    | Var id -> Symbol.name id
    | Const c -> Int32.to_string c
    | Unop unop -> sprintf "%s(%s)" (pp_unop unop.op) (pp_mexp unop.operand)
    | True -> "true"
    | False -> "false"
    | PureBinop binop ->
      sprintf
        "(%s %s %s)"
        (pp_mexp binop.lhs)
        (pp_binop_pure binop.op)
        (pp_mexp binop.rhs)
    | CmpBinop binop ->
      sprintf
        "(%s %s %s)"
        (pp_mexp binop.lhs)
        (pp_binop_comp binop.op)
        (pp_mexp binop.rhs)
    | EfktBinop binop ->
      sprintf
        "(%s %s %s)"
        (pp_mexp binop.lhs)
        (pp_binop_efkt binop.op)
        (pp_mexp binop.rhs)
    | Ternary t -> sprintf "(%s ? %s : %s)" (pp_mexp t.cond) (pp_mexp t.lb) (pp_mexp t.rb)

  and pp_mexp e = pp_exp (Mark.data e)

  let rec pp_stm ?(n = 0) stm =
    let f ~n = function
      | Declare { var; typ; body } ->
        sprintf
          "%s %s;\n%s"
          (Typ._tostring typ)
          (Symbol.name var)
          (pp_program ~n:(n + 1) body)
      | Assign { var; exp } -> sprintf "%s = %s;" (Symbol.name var) (pp_mexp exp)
      | If { cond; lb; rb } ->
        sprintf
          "if (%s) {\n%s} else {\n  %s}"
          (pp_mexp cond)
          (pp_program ~n:(n + 1) lb)
          (pp_program ~n:(n + 1) rb)
      | While { cond; body } ->
        sprintf "while(%s) {\n%s}" (pp_mexp cond) (pp_program ~n:(n + 1) body)
      | Return e -> sprintf "return %s;" (pp_mexp e)
      | NakedExpr e -> sprintf "(%s);" (pp_mexp e)
      | Seq (s1, s2) -> sprintf "%s  %s" (pp_program ~n s1) (pp_program ~n s2)
      | Nop -> "nop;"
    in
    tabs n ^ f ~n stm

  and pp_program ?(n = 0) prog = pp_stm ~n (Mark.data prog) ^ "\n"

  let print_all prog = "\n\n" ^ pp_stm ~n:0 (Mark.data prog) ^ "\n"
end
