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

let aste_pure_binop_to_pbop = function
  | A.Plus -> IntOp Add
  | A.Minus -> IntOp Sub
  | A.Times -> IntOp Mul
  | A.BitAnd -> IntOp BitAnd
  | A.BitOr -> IntOp BitOr
  | A.BitXor -> IntOp BitXor
  | A.And -> LogOp And
  | A.Or -> LogOp Or
  | A.Leq -> LogOp Leq
  | A.Less -> LogOp Less
  | A.Geq -> LogOp Geq
  | A.Greater -> LogOp Greater
  | A.Eq -> LogOp Eq
  | A.Neq -> LogOp Neq
;;

type int_ebop =
  | Div
  | Mod
  | ShftL
  | ShftR

type ebop = IntOp of int_ebop

let aste_efkt_binop_to_ebop = function
  | A.Divided_by -> IntOp Div
  | A.Modulo -> IntOp Mod
  | A.ShiftL -> IntOp ShftL
  | A.ShiftR -> IntOp ShftR
;;

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

module type PRINT = sig
  val pp_pexp : pexp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end

module Print : PRINT = struct
  let pp_int_pbop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | BitAnd -> "&"
    | BitOr -> "|"
    | BitXor -> "^"
  ;;

  let pp_log_pbop = function
    | And -> "&&"
    | Or -> "||"
    | Leq -> "<="
    | Less -> "<"
    | Geq -> ">="
    | Greater -> ">"
    | Eq -> "=="
    | Neq -> "!="
  ;;

  let pp_pbop : pbop -> string = function
    | IntOp iop -> pp_int_pbop iop
    | LogOp lop -> pp_log_pbop lop
  ;;

  let pp_int_ebop = function
    | Div -> "/"
    | Mod -> "%"
    | ShftL -> "<<"
    | ShftR -> ">>"
  ;;

  let pp_ebop = function
    | IntOp iop -> pp_int_ebop iop
  ;;

  let rec pp_pexp = function
    | Const x -> Int32.to_string x
    | Temp t -> Temp.name t
    | Pbop pbop ->
      Printf.sprintf
        "(%s %s %s)"
        (pp_pexp pbop.lhs)
        (pp_pbop pbop.op)
        (pp_pexp pbop.rhs)
  ;;

  let pp_stm = function
    | MovPureExp mv -> Temp.name mv.dest ^ "  <--  " ^ pp_pexp mv.src
    | MovEfktExp mv -> Temp.name mv.dest ^ "  <--  " ^ Printf.sprintf "(%s %s %s)" (pp_pexp mv.lhs) (pp_ebop mv.ebop) (pp_pexp mv.rhs)
    | Goto l -> "goto " ^ Label.name l
    | Label l -> Label.name l
    | If ifs -> "if " ^ pp_pexp ifs.cond ^ Label.name ifs.lt ^ " else " ^ Label.name ifs.lf
    | Return e -> "return " ^ pp_pexp e
  ;;

  let rec pp_program = function
    | [] -> ""
    | stm :: stms -> pp_stm stm ^ "\n" ^ pp_program stms
  ;;
end
