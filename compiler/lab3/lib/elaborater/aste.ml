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
  | AssertFail
  | NakedCall of
      { name : Symbol.t
      ; args : mexp list
      }

and mstm = stm Mark.t

type glob =
  | Typedef of
      { told : Typ.tau
      ; tnew : Typ.tau
      }
  | Fundecl of
      { f : Symbol.t
      ; args : Symbol.t list
      ; fsig : Typ.fsig
      }
  | Fundef of
      { f : Symbol.t
      ; args : Symbol.t list
      ; fsig : Typ.fsig
      ; fdef : mstm
      }

type mglob = glob Mark.t
type program = mglob list

module Print = struct
  let repeat s n = List.map ~f:(fun _ -> s) (List.range 0 n) |> String.concat
  let tabs = repeat ""

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
        "(%s (%s))"
        (Symbol.name name)
        (List.map args ~f:(fun e -> pp_mexp e ^ ", ") |> String.concat)

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
             (pp_mstm ~n body)
         | Some e ->
           sprintf
             "%s %s=%s;\n%s"
             (Typ._tau_tostring typ)
             (Symbol.name var)
             (pp_mexp e)
             (pp_mstm ~n body))
      | Assign { var; exp } -> sprintf "%s = %s;" (Symbol.name var) (pp_mexp exp)
      | If { cond; lb; rb } ->
        sprintf
          "if (%s) {%s\n%s\n%s}\n%selse {\n%s%s\n%s}"
          (pp_mexp cond)
          (tabs n)
          (pp_mstm ~n:(n + 1) lb)
          (tabs n)
          (tabs n)
          (pp_mstm ~n:(n + 1) rb)
          (tabs (n + 1))
          (tabs n)
      | While { cond; body } ->
        sprintf "while(%s) {\n%s}" (pp_mexp cond) (pp_mstm ~n:(n + 1) body)
      | Return e -> sprintf "return %s;" (Option.value_map ~default:"" ~f:pp_mexp e)
      | NakedExpr e -> sprintf "(%s);" (pp_mexp e)
      | Seq (s1, s2) -> sprintf "%s\n%s" (pp_mstm ~n s1) (pp_mstm ~n s2)
      | Nop -> "nop;"
      | AssertFail -> "__assert_fail;"
      | NakedCall { name; args } ->
        sprintf
          "%s(%s);"
          (Symbol.name name)
          (List.map args ~f:(fun e -> pp_mexp e ^ ", ") |> String.concat)
    in
    tabs n ^ f ~n stm

  and pp_mstm ?(n = 0) prog = pp_stm ~n (Mark.data prog)

  let pp_glob ?(n = 0) = function
    | Typedef { told; tnew } ->
      sprintf "typedef %s <--- %s;" (Typ._tau_tostring told) (Typ._tau_tostring tnew)
    | Fundecl { f; args; fsig } ->
      sprintf
        "%s(%s): %s\n\n"
        (Symbol.name f)
        (List.map args ~f:(fun s -> Symbol.name s ^ ",") |> String.concat)
        (Typ._fsig_tostring fsig)
    | Fundef { f; args; fsig; fdef } ->
      sprintf
        "%s(%s): %s =\n%s\n\n"
        (Symbol.name f)
        (List.map args ~f:(fun s -> Symbol.name s ^ ",") |> String.concat)
        (Typ._fsig_tostring fsig)
        (pp_mstm ~n fdef)
  ;;

  let pp_mglob ?(n = 0) mglob = pp_glob ~n (Mark.data mglob) ^ "\n"
  let pp_program prog = List.fold prog ~init:"" ~f:(fun s g -> s ^ pp_mglob ~n:0 g)
  let print_all prog = "\n\n" ^ pp_program prog ^ "\n"
end
