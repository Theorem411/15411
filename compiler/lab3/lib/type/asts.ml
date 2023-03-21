open Core
type intop_pure =
  | Plus
  | Minus
  | Times
  | BitAnd
  | BitOr
  | BitXor

type intop_cmp =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type intop_efkt =
  | Divided_by
  | Modulo
  | ShiftL
  | ShiftR

type intop = Pure of intop_pure | Efkt of intop_efkt

type unop =
  | BitNot (*bitwise not*)
  | LogNot (*!*)

(*_ all subclasses of exp type  *)
type exp =
  | True
  | False
  | Var of
      { var : Symbol.t
      ; type_size : int
      }
  | Const of Int32.t
  | Ternary of
      { cond : exp
      ; lb : exp
      ; rb : exp
      }
  | PureBinop of
      { op : intop_pure
      ; lhs : exp
      ; rhs : exp
      }
  | EfktBinop of
      { op : intop_efkt
      ; lhs : exp
      ; rhs : exp
      }
  | CmpBinop of
      { op : intop_cmp
      ; size : int
      ; lhs : exp
      ; rhs : exp
      }
  | Unop of
      { op : unop
      ; operand : exp
      }
  | Call of
      { name : Symbol.t
      ; args : exp list
      }
  | Null
  | Deref of ptraddr
  | ArrayAccess of arraddr
  | StructAccess of ptraddr
  | Alloc of int
  | Alloc_array of
      { type_size : int
      ; len : exp
      }
  | Address of ptraddr
  | ArrayIdx of arraddr
and ptraddr = {
    start : exp
    ; off : int
  } 
and arraddr = {
    head : exp
    ; idx : exp
    ; size : int
  }

type address = Ptr of ptraddr | Arr of arraddr

type stm =
  | Declare of
      { var : Symbol.t
      ; size : int
      ; assign : exp option
      ; body : mstm
      }
  | Assign of
      { var : Symbol.t
      ; size : int
      ; exp : exp
      }
  | Asop of
      { addr : address
      ; size : int
      ; op : intop option
      ; exp : exp
      }
  | If of
      { cond : exp
      ; lb : mstm
      ; rb : mstm
      }
  | While of
      { cond : exp
      ; body : mstm
      }
  | Return of (exp * int) option
  | Nop
  | Seq of mstm * mstm
  | NakedExpr of (exp * int)
  | AssertFail
  | NakedCall of
      { name : Symbol.t
      ; args : (exp * int) list
      ; ret_size : int
      }

and mstm = stm Mark.t

type glob =
  { f : Symbol.t
  ; args : (Symbol.t * int) list
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
  | Var { var; type_size } -> sprintf "%s{%s}" (Symbol.name var) (Int.to_string type_size)
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
  | Null -> "null"
  | Deref addr -> sprintf "*%s" (pp_addr addr)
  | ArrayAccess addr -> sprintf "{%s}" (pp_addr addr)
  | StructAccess addr -> sprintf "{%s}" (pp_addr addr)
  | Alloc i -> sprintf "alloc(%s)" (Int.to_string i)
  | Alloc_array { type_size; len } ->
    sprintf "calloc(%s, %s)" (Int.to_string type_size) (pp_mexp len)
  | Address p -> 
    

  and pp_addr { ptr; off } = sprintf "%s{+%s}" (pp_mexp ptr) (Int.to_string off)
  and pp_mexp (e, _) = pp_exp e

  let rec pp_stm = function
      | Declare { var; assign; body } ->
        (match assign with
         | None -> sprintf "decl %s;\n%s" (Symbol.name var) (pp_mstm body)
         | Some (e, i) ->
           sprintf "decl %s:%s=%s;\n%s" (Symbol.name var) (Int.to_string i) (pp_mexp (e, i)) (pp_mstm body))
      | AssignToSymbol { var; exp=e, i } -> sprintf "%s = %s{%s};" (Symbol.name var) (pp_mexp (e, i)) (Int.to_string i)
      | AsopMemPure { addr; op = None; exp=e, i } ->
        sprintf "(%s) = %s{%s}" (pp_addr addr) (pp_mexp (e, i)) (Int.to_string i)
      | AsopMemPure { addr; op = Some o; exp=e, i } ->
        sprintf "(%s) %s= %s{%s}" (pp_addr addr) (pp_binop_pure o) (pp_mexp (e, i)) (Int.to_string i)
      | AsopMemEfkt { addr; op = None; exp=e, i } ->
        sprintf "(%s) = %s{%s}" (pp_addr addr) (pp_mexp (e, i)) (Int.to_string i)
      | AsopMemEfkt { addr; op = Some o; exp=e, i } ->
        sprintf "(%s) %s= %s{%s}" (pp_addr addr) (pp_binop_efkt o) (pp_mexp (e,i)) (Int.to_string i)
      | If { cond=c, i; lb; rb} ->
        sprintf
          "if (%s{%s}) {\n%s\n}\nelse {\n%s\n}"
          (pp_mexp (c, i))
          (Int.to_string i)
          (pp_mstm lb)
          (pp_mstm rb)
      | While { cond=c, i; body } ->
        sprintf "while(%s{%s}) {\n%s}" (pp_mexp (c, i)) (Int.to_string i) (pp_mstm body)
      | Return eopt -> (match eopt with 
        | None -> "return"
        | Some (e', i) -> sprintf "return %s{%s}" (pp_mexp (e', i)) (Int.to_string i))
      | NakedExpr (e, i) -> sprintf "(%s{%s});" (pp_mexp (e, i)) (Int.to_string i)
      | Seq (s1, s2) -> sprintf "%s\n%s" (pp_mstm s1) (pp_mstm s2)
      | Nop -> "nop;"
      | AssertFail -> "__assert_fail;"
      | NakedCall { name; args } ->
        sprintf
          "%s(%s);"
          (Symbol.name name)
          (List.map args ~f:(fun (e, i) -> sprintf "%s{%s}" (pp_mexp (e,i)) (Int.to_string i)) |> String.concat ~sep:", ")

  and pp_mstm prog = pp_stm (Mark.data prog)

  let pp_glob { f; args; fdef } =
    sprintf
      "%s(%s) =\n%s\n\n"
      (Symbol.name f)
      (List.map args ~f:(fun (s, i) -> sprintf "%s:%s," (Symbol.name s) (Int.to_string i)) |> String.concat)
      (pp_mstm fdef)
  ;;

  let pp_mglob mglob = pp_glob (Mark.data mglob) ^ "\n"
  let pp_program prog = List.fold prog ~init:"" ~f:(fun s g -> s ^ pp_mglob g)
  let print_all prog = "\n\n" ^ pp_program prog ^ "\n"
end
