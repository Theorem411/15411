module A = Aste

type pbop = 
| Add
| Sub
| Mul
| BitAnd
| BitOr
| BitXor

type ebop = 
  | Div
  | Mod
  | ShftL
  | ShftR

type cbop =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq
  
type unop = 
  | BitNot
(*_ pure expression: binary operator can only be pure operator *)
type pexp =
  | Const of Int32.t
  | Temp of Temp.t
  | Binop of
      { op : pbop
      ; lhs : pexp
      ; rhs : pexp
      }
  | Cmpop of 
      { op : cbop
      ; lhs : pexp
      ; rhs : pexp
      }
  | Unop of 
      { op : unop
      ; p : pexp
      }
and cond = 
{ cmp : cbop
; p1 : pexp
; p2 : pexp
}
and stm =
| If of
    { cond : cond
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
  let pp_pbop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | BitAnd -> "&"
    | BitOr -> "|"
    | BitXor -> "^"
  ;;

  let pp_cbop = function
    | Leq -> "<="
    | Less -> "<"
    | Geq -> ">="
    | Greater -> ">"
    | Eq -> "=="
    | Neq -> "!="
  ;;

  let pp_ebop = function
    | Div -> "/"
    | Mod -> "%"
    | ShftL -> "<<"
    | ShftR -> ">>"
  ;;

  let pp_unop = function 
    | BitNot -> "~"

  let rec pp_pexp = function
    | Const x -> Int32.to_string x
    | Temp t -> Temp.name t
    | Binop pbop ->
      Printf.sprintf
        "(%s %s %s)"
        (pp_pexp pbop.lhs)
        (pp_pbop pbop.op)
        (pp_pexp pbop.rhs)
    | Unop uop -> 
      Printf.sprintf
        "%s%s"
        (pp_unop uop.op)
        (pp_pexp uop.p)
  ;;


  let pp_if_cond (cond : cond) =
    Printf.sprintf 
      "(%s %s %s)" 
      (pp_pexp cond.p1) 
      (pp_cbop cond.cmp) 
      (pp_pexp cond.p2)

  let pp_stm = function
    | MovPureExp mv -> Temp.name mv.dest ^ "  <--  " ^ pp_pexp mv.src
    | MovEfktExp mv -> Temp.name mv.dest ^ "  <--  " ^ Printf.sprintf "(%s %s %s)" (pp_pexp mv.lhs) (pp_ebop mv.ebop) (pp_pexp mv.rhs)
    | Goto l -> "goto " ^ Label.name l
    | Label l -> Label.name l
    | If ifs -> "if " ^ (pp_if_cond ifs.cond) ^ Label.name ifs.lt ^ " else " ^ Label.name ifs.lf
    | Return e -> "return " ^ pp_pexp e
  ;;

  let rec pp_program = function
    | [] -> ""
    | stm :: stms -> pp_stm stm ^ "\n" ^ pp_program stms
  ;;
end
