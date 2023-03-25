open Core
module A = Aste_l4

type intop_pure =
  | Plus
  | Minus
  | Times
  | BitAnd
  | BitOr
  | BitXor

type intop_efkt =
  | Divided_by
  | Modulo
  | ShiftL
  | ShiftR

type intop =
  | Pure of intop_pure
  | Efkt of intop_efkt

type unop =
  | BitNot (*bitwise not*)
  | LogNot (*!*)

type intop_cmp =
  | Leq
  | Less
  | Geq
  | Greater
  | Eq
  | Neq

type ptrop_cmp =
  | PtrEq
  | PtrNeq

(*_ operator conversion function *)
let intop_pure = function
  | A.Plus -> Plus
  | A.Minus -> Minus
  | A.Times -> Times
  | A.BitAnd -> BitAnd
  | A.BitOr -> BitOr
  | A.BitXor -> BitXor
;;

let intop_efkt = function
  | A.Divided_by -> Divided_by
  | A.Modulo -> Modulo
  | A.ShiftL -> ShiftL
  | A.ShiftR -> ShiftR
;;

let intop = function 
  | A.Pure o -> Pure (intop_pure o)
  | A.Efkt o -> Efkt (intop_efkt o)
;;
let intop_cmp = function
  | A.Leq -> Leq
  | A.Less -> Less
  | A.Geq -> Geq
  | A.Greater -> Greater
  | A.Eq -> Eq
  | A.Neq -> Neq
;;

let ptrop_cmp = function
  | A.Eq -> PtrEq
  | A.Neq -> PtrNeq
  | _ -> failwith "no you can not"
;;

let unop = function
  | A.LogNot -> LogNot
  | A.BitNot -> BitNot
;;

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
      ; lhs : exp
      ; rhs : exp
      }
  | CmpPointer of
      { op : ptrop_cmp
      ; lhs : ptraddr
      ; rhs : ptraddr
      }
  | Unop of
      { op : unop
      ; operand : exp
      }
  | Call of
      { name : Symbol.t
      ; args : (exp * int) list
      }
  | Deref of ptraddr
  | ArrayAccess of arraddr
  | StructAccess of ptraddr
  | Alloc of int
  | Alloc_array of
      { type_size : int
      ; len : exp
      }
  | PtrAddr of ptraddr
  | ArrAddr of arraddr

and arraddr =
  { head : exp
  ; idx : exp
  ; size : int
  ; extra : int
  }

and ptraddr =
  | Ptr of
      { start : exp
      ; off : int
      }
  | Null

let ptraddr_exn = function 
  | PtrAddr ptraddr -> ptraddr
  | _ -> failwith "no you can not"
;;
let arraddr_exn = function 
  | ArrAddr arraddr -> arraddr 
  | _ -> failwith "no you can not"
;;

(*_ stm type *)
type stm =
  | Declare of
      { var : Symbol.t
      ; size : int
      ; assign : exp option
      ; body : stm
      }
  | Assign of
      { var : Symbol.t
      ; size : int
      ; exp : exp
      }
  | Asop of
      { dest : exp
      ; size : int
      ; op : intop option
      ; exp : exp
      }
  | If of
      { cond : exp
      ; lb : stm
      ; rb : stm
      }
  | While of
      { cond : exp
      ; body : stm
      }
  | Return of (exp * int) option
  | Nop
  | Seq of stm * stm
  | NakedExpr of (exp * int)
  | AssertFail
  | NakedCall of
      { name : Symbol.t
      ; args : (exp * int) list
      }

type glob =
  { f : Symbol.t
  ; args : (Symbol.t * int) list
  ; fdef : stm
  }

type program = glob list

module Print = struct
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

  let pp_binop = function
    | Pure o -> pp_binop_pure o
    | Efkt o -> pp_binop_efkt o
  ;;

  let pp_unop = function
    | BitNot -> "~"
    | LogNot -> "!"
  ;;

  let pp_ptrop_cmp = function
    | PtrEq -> "=="
    | PtrNeq -> "!="
  ;;

  let rec pp_exp = function
    | Var { var; type_size } ->
      sprintf "%s{%s}" (Symbol.name var) (Int.to_string type_size)
    | Const c -> Int32.to_string c
    | Unop unop -> sprintf "%s(%s)" (pp_unop unop.op) (pp_exp unop.operand)
    | True -> "true"
    | False -> "false"
    | PureBinop binop ->
      sprintf "(%s %s %s)" (pp_exp binop.lhs) (pp_binop_pure binop.op) (pp_exp binop.rhs)
    | CmpBinop binop ->
      sprintf "(%s %s %s)" (pp_exp binop.lhs) (pp_binop_comp binop.op) (pp_exp binop.rhs)
    | CmpPointer { op; lhs; rhs } ->
      sprintf "(%s %s %s)" (pp_ptraddr lhs) (pp_ptrop_cmp op) (pp_ptraddr rhs)
    | EfktBinop binop ->
      sprintf "(%s %s %s)" (pp_exp binop.lhs) (pp_binop_efkt binop.op) (pp_exp binop.rhs)
    | Ternary t -> sprintf "(%s ? %s : %s)" (pp_exp t.cond) (pp_exp t.lb) (pp_exp t.rb)
    | Call { name; args } ->
      sprintf
        "(%s (%s))"
        (Symbol.name name)
        (List.map args ~f:(fun (e, i) -> sprintf "%s[%s]" (pp_exp e) (Int.to_string i)) |> String.concat ~sep:", ")
    | Deref addr -> sprintf "(%s)" (pp_ptraddr addr)
    | ArrayAccess addr -> sprintf "arr[%s]" (pp_arraddr addr)
    | StructAccess addr -> sprintf "(%s)" (pp_ptraddr addr)
    | Alloc i -> sprintf "alloc(%s)" (Int.to_string i)
    | Alloc_array { type_size; len } ->
      sprintf "calloc(%s, %s)" (Int.to_string type_size) (pp_exp len)
    | PtrAddr ptraddr -> pp_ptraddr ptraddr
    | ArrAddr arraddr -> pp_arraddr arraddr

  and pp_ptraddr = function
  | Ptr { start; off } -> sprintf "%s:%s" (pp_exp start) (Int.to_string off)
  | Null -> "NULL"
  and pp_arraddr { head; idx; size; extra } = 
    sprintf
      "%s:%s:%s:%s"
      (pp_exp head)
      (pp_exp idx)
      (Int.to_string size)
      (Int.to_string extra)
  ;;

  let rec pp_stm = function
    | Declare { var; size; assign; body } ->
      (match assign with
       | None -> sprintf "decl %s;\n%s" (Symbol.name var) (pp_stm body)
       | Some e ->
         sprintf
           "decl %s[%s]=%s;\n%s"
           (Symbol.name var)
           (Int.to_string size)
           (pp_exp e)
           (pp_stm body))
    | Assign { var; size; exp } ->
      sprintf "%s[%s] = %s;" (Symbol.name var) (Int.to_string size) (pp_exp exp)
    | Asop { dest; size; op = None; exp } ->
      sprintf "(%s) = %s[%s]" (pp_exp dest) (pp_exp exp) (Int.to_string size)
    | Asop { dest; size; op = Some o; exp } ->
      sprintf
        "(%s) %s= %s[%s]"
        (pp_exp dest)
        (pp_binop o)
        (pp_exp exp)
        (Int.to_string size)
    | If { cond; lb; rb } ->
      sprintf "if (%s) {\n%s\n}\nelse {\n%s\n}" (pp_exp cond) (pp_stm lb) (pp_stm rb)
    | While { cond; body } -> sprintf "while(%s) {\n%s}" (pp_exp cond) (pp_stm body)
    | Return eopt ->
      (match eopt with
       | None -> "return"
       | Some (e, i) -> sprintf "return %s[%s]" (pp_exp e) (Int.to_string i))
    | NakedExpr (e, i) -> sprintf "%s[%s];" (pp_exp e) (Int.to_string i)
    | Seq (s1, s2) -> sprintf "%s\n%s" (pp_stm s1) (pp_stm s2)
    | Nop -> "nop;"
    | AssertFail -> "__assert_fail;"
    | NakedCall { name; args } ->
      sprintf
        "%s(%s);"
        (Symbol.name name)
        (List.map args ~f:(fun (e, i) -> sprintf "%s[%s]" (pp_exp e) (Int.to_string i))
        |> String.concat ~sep:", ")

  let pp_glob { f; args; fdef } =
    sprintf
      "%s(%s) =\n%s\n\n"
      (Symbol.name f)
      (List.map args ~f:(fun (s, i) -> sprintf "%s:%s," (Symbol.name s) (Int.to_string i))
      |> String.concat)
      (pp_stm fdef)
  ;;
  let pp_program prog = List.fold prog ~init:"" ~f:(fun s g -> s ^ pp_glob g)
  let print_all prog = "\n\n" ^ pp_program prog ^ "\n"
end
