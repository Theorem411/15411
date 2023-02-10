(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *   - Use a linear, not quadratic, algorithm.
 *
 * Implements a "convenient munch" algorithm
 *)

open Core
module T = Tree
module AS = Assem

let munch_op = function
  | T.Add -> AS.Add
  | T.Sub -> AS.Sub
  | T.Mul -> AS.Mul
  | T.Div -> AS.Div
  | T.Mod -> AS.Mod
;;

(* munch_exp dest exp
 *
 * Generates instructions for dest <-- exp.
 *)
let munch_exp : AS.operand -> T.exp -> AS.instr list =
  (* munch_exp_acc dest exp rev_acc
   *
   * Suppose we have the statement:
   *   dest <-- exp
   *
   * If the codegened statements for this are:
   *   s1; s2; s3; s4;
   *
   * Then this function returns the result:
   *   s4 :: s3 :: s2 :: s1 :: rev_acc
   *
   * I.e., rev_acc is an accumulator argument where the codegen'ed
   * statements are built in reverse. This allows us to create the
   * statements in linear time rather than quadratic time (for highly
   * nested expressions).
   *)
  let rec munch_exp_acc (dest : AS.operand) (exp : T.exp) (rev_acc : AS.instr list)
      : AS.instr list
    =
    match exp with
    | T.Const n -> AS.Mov { dest; src = AS.Imm n } :: rev_acc
    | T.Temp t -> AS.Mov { dest; src = AS.Temp t } :: rev_acc
    | T.Binop binop -> munch_binop_acc dest (binop.op, binop.lhs, binop.rhs) rev_acc
  (* munch_binop_acc dest (binop, e1, e2) rev_acc
   *
   * generates instructions to achieve dest <- e1 binop e2
   *
   * Much like munch_exp, this returns the result of appending the
   * instructions in reverse to the accumulator argument, rev_acc.
   *)
  and munch_binop_acc
      (dest : AS.operand)
      ((binop, e1, e2) : T.binop * T.exp * T.exp)
      (rev_acc : AS.instr list)
      : AS.instr list
    =
    let op = munch_op binop in
    let t1 = AS.Temp (Temp.create ()) in
    let t2 = AS.Temp (Temp.create ()) in
    let rev_acc' = rev_acc |> munch_exp_acc t1 e1 |> munch_exp_acc t2 e2 in
    AS.Binop { op; dest; lhs = t1; rhs = t2 } :: rev_acc'
  in
  fun dest exp ->
    (* Since munch_exp_acc returns the reversed accumulator, we must
     * reverse the list before returning. *)
    List.rev (munch_exp_acc dest exp [])
;;

(* munch_stm : T.stm -> AS.instr list *)
(* munch_stm stm generates code to execute stm *)
let munch_stm = function
  | T.Move mv -> munch_exp (AS.Temp mv.dest) mv.src
  | T.Return e ->
    (* return e is implemented as %eax <- e *)
    munch_exp (AS.Reg AS.EAX) e
;;

let upsert (context : (AS.operand * AS.operand) list) (d, s) = (d, s) :: context

let rec lookup (context : (AS.operand * AS.operand) list) (x : AS.operand) =
  match context with
  | [] -> None
  | (y, v) :: rest -> if AS.equal_operand x y then Some v else lookup rest x
;;

let from_ctx context = function
  | AS.Imm n -> AS.Imm n
  | c ->
    (match lookup context c with
    | None -> c
    | Some v -> v)
;;

let add_text ?(text = "\t\t\t\t\t.skipped ") command =
  AS.Comment (text ^ AS.format command)
;;

let transform (ctx, command) : AS.instr * (AS.operand * AS.operand) list =
  match command with
  | AS.Mov { dest = AS.Temp td; src = src } ->
    add_text command, upsert ctx (AS.Temp td, from_ctx ctx src)
  | AS.Mov { dest = AS.Reg r; src = AS.Imm n } -> 
    command, upsert ctx (AS.Reg r, AS.Imm n)
  | AS.Mov { dest = AS.Reg td; src = AS.Temp tc } ->
    ( AS.Mov { dest = AS.Reg td; src = from_ctx ctx (AS.Temp tc) }
    , upsert ctx (AS.Reg td, from_ctx ctx (AS.Temp tc)) )
  | AS.Binop { op; dest = d; lhs = l; rhs = r } ->
    let new_l = from_ctx ctx l in
    let new_r = from_ctx ctx r in
    (* TODO: Decide if Add evalutation of operations here *)
    AS.Binop { op; dest = d; lhs = new_l; rhs = new_r }, ctx
  | _ -> command, ctx
;;

let transform2 (context, oldcommands) command =
  let command_to_add, new_context = transform (context, command) in
  new_context, command_to_add :: oldcommands
;;

let prop (commands : AS.instr list) : AS.instr list =
  let _, res = List.fold commands ~init:([], []) ~f:transform2 in
  let f = (function AS.Comment _ -> false | _ -> true) in
  List.filter (List.rev res) ~f:f
;;

(* To codegen a series of statements, just concatenate the results of
 * codegen-ing each statement.*)
let codegen inp = prop (List.concat_map ~f:munch_stm inp)