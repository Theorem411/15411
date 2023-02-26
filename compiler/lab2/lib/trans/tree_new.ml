open Core
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
| MovFuncApp of 
    { dest : Temp.t option
    ; fname : Symbol.t
    ; args : pexp list}
| Return of pexp option
| AssertFail
type fspace = { fname : Symbol.t
              ; fdef : stm list}
type program = fspace list
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
    | Cmpop cop -> 
      Printf.sprintf
        "(%s %s %s)"
        (pp_pexp cop.lhs)
        (pp_cbop cop.op)
        (pp_pexp cop.rhs)
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
    | MovFuncApp { dest; fname; args; } -> (
        let fstr = Symbol.name fname in
        let argstr = List.fold args ~init:"" ~f:(fun acc -> fun e -> acc ^ pp_pexp e ^ ", ") in
        match dest with
        | None -> Printf.sprintf "%s(%s)" fstr argstr
        | Some d -> Printf.sprintf "%s  <--  %s(%s)" (Temp.name d) fstr argstr
      )
    | Goto l -> "goto " ^ Label.name l
    | Label l -> Label.name l
    | If ifs -> "if " ^ (pp_if_cond ifs.cond) ^ Label.name ifs.lt ^ " else " ^ Label.name ifs.lf
    | Return eopt -> (
      match eopt with 
      | None -> "return"
      | Some e -> "return " ^ pp_pexp e)
      
    | AssertFail -> "assertfail"
  ;;
  let pp_stms (stms : stm list) = 
    List.fold ~init:"" ~f:(fun acc -> fun s -> acc ^ pp_stm s ^ "\n") stms 
  ;;
  let rec pp_program (prog : program) =
    match prog with
    | [] -> ""
    | {fname; fdef} :: prog' -> 
        Printf.sprintf "%s:\n%s\n----\n%s" 
        (Symbol.name fname) (pp_stms fdef) (pp_program prog')
  ;;
end
