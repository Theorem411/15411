open Core
module AS = Assem_l4
module V = Graph.Vertex
module R = Register
module Regalloc = Regalloc
module TM = Temp.Map

let to_int = function
  | AS.L -> 8
  | AS.S -> 4
;;

(* returns move commands to move from rdi -> t_a1, rsi -> t_a2, ...  *)
let do_stack_moves
    (updater : Temp.t -> Regalloc.reg_or_spill)
    (args : (Temp.t * AS.size) list)
    offset
  =
  let stack_args = List.drop args 6 in
  let stack_refs =
    List.concat_mapi stack_args ~f:(fun i (t, sz) ->
        let d = updater t in
        match d with
        | Reg reg ->
          [ X86.BinCommand
              { op = Mov
              ; dest = X86.Reg { reg; size = to_int sz }
              ; src = X86.Stack (offset + i)
              ; size = X86.to_size sz
              }
          ]
        | Spl j ->
          let sz = X86.to_size sz in
          [ X86.BinCommand
              { op = Mov
              ; dest = sz |> X86.get_free
              ; src = X86.Stack (offset + i)
              ; size = sz
              }
          ; X86.BinCommand
              { op = Mov; dest = X86.Stack j; src = X86.get_free sz; size = sz }
          ])
  in
  stack_refs
;;

let callee_handle (reg_map : Regalloc.reg_or_spill TM.t) =
  let calee_saved = Regalloc.callee_save reg_map in
  let callee_regs =
    if List.mem calee_saved R.RBP ~equal:R.equal_reg_enum
    then calee_saved
    else R.RBP :: calee_saved
  in
  (* save them into stack *)
  let callee_start =
    List.map callee_regs ~f:(fun r ->
        X86.UnCommand { op = X86.Pushq; src = X86.Reg { reg = r; size = 8 } })
  in
  let callee_finish =
    List.map (List.rev callee_regs) ~f:(fun r ->
        X86.UnCommand { op = X86.Popq; src = X86.Reg { reg = r; size = 8 } })
  in
  callee_regs, callee_start, callee_finish
;;

let handle_sub (sub_c : int) =
  if sub_c = 0
  then [], []
  else
    ( [ X86.BinCommand
          { op = X86.Sub
          ; dest = X86.Reg { reg = R.RSP; size = 8 }
          ; src = X86.Imm (Int64.of_int_exn sub_c)
          ; size = X86.Q
          }
      ]
    , [ X86.BinCommand
          { op = X86.Add
          ; dest = X86.Reg { reg = R.RSP; size = 8 }
          ; src = X86.Imm (Int64.of_int_exn sub_c)
          ; size = X86.Q
          }
      ] )
;;

let get_function_be
    ((fname, args) : Symbol.t * (Temp.t * AS.size) list)
    ((reg_map, updater) : Regalloc.reg_or_spill TM.t * (Temp.t -> Regalloc.reg_or_spill))
    (n : int) (*stack cells used*)
  =
  let cee_regs, cee_start, cee_finish = callee_handle reg_map in
  let cee_regs_cnt = List.length cee_regs in
  (* TODO OPTIMIZE DIVISIONS BY 8 *)
  let sub_c : int = if (n + cee_regs_cnt) % 2 = 0 then (n * 8) + 8 else n * 8 in
  (* total size of frame (added regs)*)
  let total_size = sub_c + (cee_regs_cnt * 8) in
  let offset = (total_size / 8) + 1 in
  let stack_moves = do_stack_moves updater args offset in
  let sub_enter, sub_exit = handle_sub sub_c in
  (* function labels *)
  let enter =
    [ X86.Directive (sprintf ".globl %s" (Symbol.name fname))
    ; X86.Directive (sprintf ".type\t%s, @function" (Symbol.name fname))
    ; X86.FunName (Symbol.name fname)
    ]
    @ cee_start
    @ sub_enter
  in
  let ret_label = Label.create () in
  let exit =
    [ X86.Comment ("\treturn label of " ^ Symbol.name fname); X86.Lbl ret_label ]
    @ sub_exit
    @ cee_finish
    @ [ X86.Ret ]
  in
  enter, exit, (stack_moves, offset), ret_label
;;