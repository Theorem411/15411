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
let do_arg_moves
    (reg_map : Regalloc.reg_or_spill TM.t)
    (args : (Temp.t * AS.size) list)
    total_size
  =
  let reg_args, stack_args = List.take args 6, List.drop args 6 in
  let reg_moves =
    let srcs =
      List.mapi reg_args ~f:(fun i (_, sz) ->
          X86.to_size sz, X86.Reg (R.arg_i_to_reg (to_int sz) i))
    in
    let dests =
      List.map reg_args ~f:(fun (t, sz) ->
          match TM.find_exn reg_map t with
          | Spl i -> X86.Stack i
          | Reg reg -> X86.Reg { reg; size = to_int sz })
    in
    let create d (sz, s) = X86.BinCommand { op = Mov; dest = d; src = s; size = sz } in
    List.map2_exn dests srcs ~f:create
  in
  let stack_refs =
    List.concat_mapi stack_args ~f:(fun i (t, sz) ->
        let d = TM.find_exn reg_map t in
        match d with
        | Reg reg ->
          [ X86.BinCommand
              { op = Mov
              ; dest = X86.Reg { reg; size = to_int sz }
              ; src = X86.Stack (total_size + 8 + (8 * i))
              ; size = X86.to_size sz
              }
          ]
        | Spl i ->
          let sz = X86.to_size sz in
          [ X86.BinCommand
              { op = Mov
              ; dest = sz |> X86.get_free
              ; src = X86.Stack (total_size + 8 + (8 * i))
              ; size = sz
              }
          ; X86.BinCommand
              { op = Mov; dest = X86.Stack i; src = X86.get_free sz; size = sz }
          ])
  in
  reg_moves @ stack_refs
;;

let callee_handle (reg_map : Regalloc.reg_or_spill TM.t) =
  let calee_saved = Regalloc.callee_save reg_map in
  let callee_regs =
    if List.mem calee_saved R.RBP ~equal:R.equal_reg_enum
    then R.RBP :: calee_saved
    else calee_saved
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
    (reg_map : Regalloc.reg_or_spill TM.t)
    (n : int) (*stack cells used*)
  =
  let cee_regs, cee_start, cee_finish = callee_handle reg_map in
  let cee_regs_cnt = List.length cee_regs in
  let sub_c : int = if (n + cee_regs_cnt) % 2 = 0 then (n * 8) + 8 else n * 8 in
  (* total size of frame (added regs)*)
  let move_locals = do_arg_moves reg_map args (sub_c + (cee_regs_cnt * 8)) in
  let sub_enter, sub_exit = handle_sub sub_c in
  (* function labels *)
  let enter =
    [ X86.Directive (sprintf ".globl %s" (Symbol.name fname))
    ; X86.Directive (sprintf ".type\t%s, @function" (Symbol.name fname))
    ; X86.FunName (Symbol.name fname)
    ]
    @ cee_start
    @ [ X86.BinCommand
          { op = Mov
          ; dest = X86.Reg { reg = R.RBP; size = 8 }
          ; src = X86.Reg { reg = R.RSP; size = 8 }
          ; size = X86.Q
          }
      ]
    @ sub_enter
    @ move_locals
  in
  let ret_label = Label.create () in
  let exit =
    [ X86.Comment ("\treturn label of " ^ Symbol.name fname); X86.Lbl ret_label ]
    @ sub_exit
    @ cee_finish
    @ [ X86.Ret ]
  in
  enter, exit, ret_label
;;