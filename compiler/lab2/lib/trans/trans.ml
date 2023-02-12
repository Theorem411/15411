(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)
open Core
module A = Ast
module S = Symbol.Map
module T = Tree

(* let trans_binop = function
  | A.Plus -> T.Add
  | A.Minus -> T.Sub
  | A.Times -> T.Mul
  | A.Divided_by -> T.Div
  | A.Modulo -> T.Mod
  | _ -> raise (Failure "not supported yet")
;;

let trans_unop = function
  (* unary to binary!*)
  | A.Negative -> T.Sub
  | _ -> raise (Failure "not supported yet")
;; *)

(* let rec trans_exp env = function
  (* after type-checking, id must be declared; do not guard lookup *)
  | A.Var id -> T.Temp (S.find_exn env id)
  | A.Const c -> T.Const c
  | A.Binop binop ->
    T.Binop
      { op = trans_binop binop.op
      ; lhs = trans_mexp env binop.lhs
      ; rhs = trans_mexp env binop.rhs
      }
  | A.Unop { op = A.Negative; operand = e } ->
    T.Binop
      { op = trans_unop A.Negative; lhs = T.Const Int32.zero; rhs = trans_mexp env e }
  | _ -> raise (Failure "not supported yet")

and trans_mexp env mexp = trans_exp env (Mark.data mexp) *)

(* translate the statement *)
(* let rec trans_stms (env : Temp.t S.t) (ast : A.stm list) : T.stm list =
  match ast with
  | A.Declare d :: stms ->
    (match d with
    | A.New_var _ -> trans_stms env stms
    (* | A.Init (id,_, e) -> trans_stms env (A.Assign (Mark.data id, e) :: stms)) *)
    | _ -> raise (Failure "should not happen"))
  | A.Assign (A.Var id, _,  e) :: stms ->
    let t = Temp.create () in
    let env' = S.set env ~key:id ~data:t in
    T.Move { dest = t; src = trans_mexp env e } :: trans_stms env' stms
  | A.Return e :: _ ->
    (* ignore code after return *)
    [ T.Return (trans_mexp env e) ]
  | [] ->
    (* There must be a return. *)
    assert false
  | _ -> raise (Failure "not implemented yet")
;; *)
let trans_stms (_ : Temp.t S.t) (_ : A.stm list) : T.stm list = raise (Failure "not implemented yet")

let trans_mstms (env : Temp.t S.t) (ast : A.program) : T.program =
  trans_stms env (List.map ~f:Mark.data ast)
;;

let translate (stms : A.program) : T.program = trans_mstms S.empty stms
