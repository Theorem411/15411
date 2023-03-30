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
  | Alloc of
      { dest : Temp.t
      ; size : int
      }
  | Calloc of
      { dest : Temp.t
      ; len : mpexp
      ; typ : int
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

  let pp_ebop = function
    | Div -> "/"
    | Mod -> "%"
    | ShftL -> "<<"
    | ShftR -> ">>"
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

  and pp_mpexp ((e, _) : mpexp) = pp_pexp e

  let pp_if_cond = function
    | LCond { cmp; p1; p2 } ->
      sprintf "(%s %s(%s) %s)" (pp_mpexp p1) (pp_cbop cmp) "large" (pp_mpexp p2)
    | SCond { cmp; p1; p2 } ->
      sprintf "(%s %s(%s) %s)" (pp_mpexp p1) (pp_cbop cmp) "small" (pp_mpexp p2)
  ;;

  let pp_stm (stm : stm) =
    match stm with
    | If { cond; lt; lf } ->
      "if " ^ pp_if_cond cond ^ Label.name lt ^ " else " ^ Label.name lf
    | Goto l -> "goto " ^ Label.name l
    | Label l -> Label.name l
    | MovEfktExp { dest; ebop; lhs; rhs } ->
      Temp.name dest
      ^ "  <--  "
      ^ Printf.sprintf "(%s %s %s)" (pp_mpexp lhs) (pp_ebop ebop) (pp_mpexp rhs)
    | MovPureExp { dest; src } -> Temp.name dest ^ "  <--  " ^ pp_mpexp src
    | MovFuncApp { dest; fname; args } ->
      let fstr = Symbol.name fname in
      let argstr = List.fold args ~init:"" ~f:(fun acc e -> acc ^ pp_mpexp e ^ ", ") in
      (match dest with
       | None -> Printf.sprintf "%s(%s)" fstr argstr
       | Some (d, _) -> Printf.sprintf "%s  <--  %s(%s)" (Temp.name d) fstr argstr)
    | MovToMem { mem; src } -> sprintf "(%s) <-- %s" (Temp.name mem) (pp_mpexp src)
    | Return eopt ->
      (match eopt with
      | None -> "return"
      | Some e -> "return " ^ pp_mpexp e)
    | AssertFail -> "assertfail"
    | Alloc { dest; size } ->
      sprintf "%s <-- alloc (%s)" (Temp.name dest) (Int.to_string size)
    | Calloc { dest; len; typ } ->
      sprintf "%s <-- calloc (%s, %s)" (Temp.name dest) (pp_mpexp len) (Int.to_string typ)
  ;;

  let pp_jump = function
    | JRet -> "ret"
    | JCon { lt; lf } -> sprintf "t:%s|f:%s" (Label.name lt) (Label.name lf)
    | JUncon l -> sprintf "goto %s" (Label.name l)
  ;;

  let pp_block ({ label; block; jump } : block) =
    sprintf
      "%s\n%s\n-------[%s]---------"
      (Label.name_bt label)
      (List.map block ~f:pp_stm |> String.concat ~sep:"\n")
      (pp_jump jump)
  ;;

  let pp_fspace ({ fname; args; fdef } : fspace_block) =
    sprintf
      "func %s(%s):\n%s"
      (Symbol.name fname)
      (List.map args ~f:(fun (t, i) -> sprintf "%s[%s]" (Temp.name t) (Int.to_string i))
      |> String.concat ~sep:", ")
      (List.map fdef ~f:pp_block |> String.concat ~sep:"\n")
  ;;

  let pp_program (prog : program) =
    List.map prog ~f:pp_fspace |> String.concat ~sep:"\n\n"
  ;;
end
