module A = Aste
type int_pbop = 
| Add
| Sub
| Mul
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
type ebop = 
| IntOp of int_ebop

let aste_efkt_binop_to_ebop = function
    | A.Divided_by -> IntOp Div
    | A.Modulo -> IntOp Mod

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
    { cond : pexp  (*_ cond should only be typed-checked to be boolean-only *)
    ; lt : Label.t
    ; lf : Label.t}
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
let pp_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
;;

let rec pp_pexp = function
    | Const x -> Int32.to_string x
    | Temp t -> Temp.name t
    | Binop binop ->
    Printf.sprintf
        "(%s %s %s)"
        (pp_exp binop.lhs)
        (pp_binop binop.op)
        (pp_exp binop.rhs)
;;

let pp_stm = function
    | Move mv -> Temp.name mv.dest ^ "  <--  " ^ pp_exp mv.src
    | Return e -> "return " ^ pp_exp e
;;

let rec pp_program = function
    | [] -> ""
    | stm :: stms -> pp_stm stm ^ "\n" ^ pp_program stms
;;
end