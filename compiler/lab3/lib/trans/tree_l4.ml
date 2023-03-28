open Core
module A = Asts

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

type ibop =
  | Pure of pbop
  | Efkt of ebop

type cbop =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type unop = BitNot

(*_ pure expression: binary operator can only be pure operator *)
type pexp =
  | Const of Int32.t
  | Temp of Temp.t
  | Binop of
      { op : pbop
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | Cmpop of
      { op : cbop
      ; size : int
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | Unop of
      { op : unop
      ; p : mpexp
      }
  | Mem of addr
  | Addr of addr
  | Alloc of int
  | Calloc of
      { len : mpexp
      ; typ : int
      }

and ptraddr =
  { start : mpexp
  ; off : int
  }

and arraddr =
  { head : mpexp
  ; idx : mpexp
  ; typ_size : int
  ; extra : int
  }

and addr =
  | Ptr of ptraddr
  | Null
  | Arr of arraddr

and mpexp = pexp * int

type cond =
  | LCond of
      { cmp : cbop
      ; p1 : mpexp
      ; p2 : mpexp
      }
  | SCond of
      { cmp : cbop
      ; p1 : mpexp
      ; p2 : mpexp
      }

type stm =
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
      ; lhs : mpexp
      ; rhs : mpexp
      }
  | MovPureExp of
      { dest : Temp.t
      ; src : mpexp
      }
  | MovFuncApp of
      { dest : (Temp.t * int) option
      ; fname : Symbol.t
      ; args : mpexp list
      }
  | MovToMem of
      { (*_ this means deref the lhs *)
        mem : Temp.t
      ; src : mpexp
      }
  | Return of mpexp option
  | AssertFail

type jump_t =
  | JRet
  | JCon of
      { lt : Label.t
      ; lf : Label.t
      }
  | JUncon of Label.t

type block =
  { label : Label.bt
  ; block : stm list
  ; jump : jump_t
  }

type fspace_block =
  { fname : Symbol.t
  ; args : (Temp.t * int) list
  ; fdef : block list
  }

type program = fspace_block list

let size ((_, i) : mpexp) = i

module Print = struct
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

  let pp_unop = function
    | BitNot -> "~"
  ;;

  let rec pp_pexp (e : pexp) =
    match e with
    | Const const -> Int32.to_string const
    | Temp t -> Temp.name t
    | Binop { op; lhs; rhs } ->
      sprintf "%s %s %s" (pp_mpexp lhs) (pp_pbop op) (pp_mpexp rhs)
    | Cmpop { op; size; lhs; rhs } ->
      (* Implement the Cmpop case *)
      sprintf
        "%s %s(%s) %s"
        (pp_mpexp lhs)
        (pp_cbop op)
        (Int.to_string size)
        (pp_mpexp rhs)
    | Unop { op; p } -> sprintf "%s%s" (pp_unop op) (pp_mpexp p)
    | Mem Null -> "(null)"
    | Mem (Ptr { start; off }) -> sprintf "(%s, %s)" (pp_mpexp start) (Int.to_string off)
    | Mem (Arr { head; idx; typ_size; extra }) ->
      sprintf
        "(%s, %s, %s, %s)"
        (pp_mpexp head)
        (pp_mpexp idx)
        (Int.to_string typ_size)
        (Int.to_string extra)
    | Addr Null -> "null"
    | Addr (Ptr { start; off }) -> sprintf "{%s, %s}" (pp_mpexp start) (Int.to_string off)
    | Addr (Arr { head; idx; typ_size; extra }) ->
      sprintf
        "{%s, %s, %s, %s}"
        (pp_mpexp head)
        (pp_mpexp idx)
        (Int.to_string typ_size)
        (Int.to_string extra)
    | Alloc size -> sprintf "alloc(%s)" (Int.to_string size)
    | Calloc { typ; len } -> sprintf "calloc(%s, %s)" (Int.to_string typ) (pp_mpexp len)

  and pp_mpexp ((e, _) : mpexp) = pp_pexp e

  let pp_stm (stm : stm) =
    match stm with
    | If { cond; lt; lf } ->
      failwith "Not implemented"
    | Goto label ->
      (* Implement the Goto case *)
      failwith "Not implemented"
    | Label label ->
      (* Implement the Label case *)
      failwith "Not implemented"
    | MovEfktExp { dest; ebop; lhs; rhs } ->
      (* Implement the MovEfktExp case *)
      failwith "Not implemented"
    | MovPureExp { dest; src } ->
      (* Implement the MovPureExp case *)
      failwith "Not implemented"
    | MovFuncApp { dest; fname; args } ->
      (* Implement the MovFuncApp case *)
      failwith "Not implemented"
    | MovToMem { mem; src } ->
      (* Implement the MovToMem case *)
      failwith "Not implemented"
    | Return mpexp_opt ->
      (* Implement the Return case *)
      failwith "Not implemented"
    | AssertFail ->
      (* Implement the AssertFail case *)
      failwith "Not implemented"
  ;;

  let pp_program (prog : program) = failwith "no"
end
