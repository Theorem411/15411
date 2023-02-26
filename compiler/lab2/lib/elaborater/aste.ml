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
  | Call of
      { name : Symbol.t
      ; args : mexp list
      }

and mexp = exp Mark.t

type stm =
  | Declare of
      { var : Symbol.t
      ; typ : Typ.tau
      ; assign : mexp option
      ; body : mstm
      }
  | Assign of
      { var : Symbol.t
      ; exp : mexp
      }
  | If of
      { cond : mexp
      ; lb : mstm
      ; rb : mstm
      }
  | While of
      { cond : mexp
      ; body : mstm
      }
  | Return of mexp option
  | Nop
  | Seq of mstm * mstm
  | NakedExpr of mexp

and mstm = stm Mark.t

type glob =
  | Typedef of Typ.tau * Typ.tau
  | Fundecl of Symbol.t * Typ.fsig
  | Fundef of Symbol.t * Typ.fsig * mstm

type mglob = glob Mark.t
type program = mglob list

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
    | Call { name; args } ->
      sprintf
        "call %s(%s)"
        (Symbol.name name)
        (String.concat ~sep:"," (List.map args ~f:pp_mexp))

  and pp_mexp e = pp_exp (Mark.data e)

  let rec pp_stm ?(n = 0) stm =
    let f ~n = function
      | Declare { var; typ; assign; body } ->
        (match assign with
        | None ->
          sprintf
            "%s %s;\n%s"
            (Typ._tau_tostring typ)
            (Symbol.name var)
            (pp_mstm ~n:(n + 1) body)
        | Some e ->
          sprintf
            "%s %s=%s;\n%s"
            (Typ._tau_tostring typ)
            (Symbol.name var)
            (pp_mexp e)
            (pp_mstm ~n:(n + 1) body))
      | Assign { var; exp } -> sprintf "%s = %s;" (Symbol.name var) (pp_mexp exp)
      | If { cond; lb; rb } ->
        sprintf
          "if (%s) {\n%s} else {\n  %s}"
          (pp_mexp cond)
          (pp_mstm ~n:(n + 1) lb)
          (pp_mstm ~n:(n + 1) rb)
      | While { cond; body } ->
        sprintf "while(%s) {\n%s}" (pp_mexp cond) (pp_mstm ~n:(n + 1) body)
      | Return e -> sprintf "return %s;" (Option.value_map ~default:"" ~f:pp_mexp e)
      | NakedExpr e -> sprintf "(%s);" (pp_mexp e)
      | Seq (s1, s2) -> sprintf "%s  %s" (pp_mstm ~n s1) (pp_mstm ~n s2)
      | Nop -> "nop;"
    in
    tabs n ^ f ~n stm

  and pp_mstm ?(n = 0) prog = pp_stm ~n (Mark.data prog) ^ "\n"

  let pp_glob ?(n = 0) = function
    | Typedef (a, b) ->
      sprintf "typedef %s <--- %s;" (Typ._tau_tostring a) (Typ._tau_tostring b)
    | _ -> tabs n ^ ""
  ;;

  let pp_mglob ?(n = 0) mglob = pp_glob ~n (Mark.data mglob) ^ "\n"
  let pp_program prog = List.fold prog ~init:"" ~f:(fun s g -> s ^ pp_mglob ~n:0 g)
  let print_all prog = "\n\n" ^ pp_program prog ^ "\n"
end
