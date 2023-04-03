open Core
module AS = Assem_l4
module R = Register
module V = Graph.Vertex
module Custom = CustomAssembly
module Regalloc = Regalloc
module Stackalloc = Stackalloc
module TM = Temp.Map
module Helper = New_helper
(*_ DEBUGGING STAFF  *)

let debug = false
let alloc = if not debug then Regalloc.reg_alloc else Stackalloc.stack_alloc

type fspace = X86.instr list
type program = fspace list

let translate_pure get_final = function
  | AS.PureBinop { op; dest = d; lhs; rhs; size } ->
    let size = X86.to_size size in
    let d_final = get_final (d, size) in
    let lhs_final = get_final (lhs, size) in
    let rhs_final = get_final (rhs, size) in
    let is_too_big_imm inp =
      match inp with
      | X86.Imm x ->
        (match Int32.of_int64 x with
        | Some _ -> false
        | None -> true)
      | _ -> false
    in
    (match d_final, is_too_big_imm rhs_final with
    | X86.Reg _, _ ->
      [ X86.BinCommand { op = Mov; dest = d_final; src = lhs_final; size }
      ; X86.BinCommand { op = X86.pure_to_opr op; dest = d_final; src = rhs_final; size }
      ]
    | Stack _, false ->
      [ X86.BinCommand { op = Mov; dest = X86.get_free size; src = lhs_final; size }
      ; X86.BinCommand
          { op = X86.pure_to_opr op; dest = X86.get_free size; src = rhs_final; size }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_free size; size }
      ]
    | Stack _, true ->
      [ X86.BinCommand { op = Mov; dest = X86.get_free size; src = rhs_final; size }
      ; X86.BinCommand
          { op = X86.pure_to_opr op; dest = X86.get_free size; src = lhs_final; size }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_free size; size }
      ]
    | _ -> failwith "X86.Imm can not be a dest")
  | _ -> failwith "not a pure operation"
;;

let translate_efkt (get_final : AS.operand * X86.size -> X86.operand) (errLabel : Label.t)
  = function
  (* DIV / MOD *)
  | AS.EfktBinop { op = (Div | Mod) as op; dest = d; lhs; rhs } ->
    let size = X86.L in
    let d_final = get_final (d, size) in
    let lhs_final = get_final (lhs, size) in
    let rhs_final = get_final (rhs, size) in
    let right_commands =
      match rhs_final with
      | X86.Imm n ->
        [ X86.BinCommand { op = Mov; dest = X86.Reg R.eax; src = lhs_final; size }
        ; X86.BinCommand { op = Mov; dest = X86.get_free size; src = X86.Imm n; size }
        ; X86.ZeroCommand { op = X86.Cltd }
        ; X86.UnCommand { op = IDiv; src = X86.get_free size }
        ; X86.BinCommand
            { op = Mov
            ; dest = d_final
            ; src =
                (match op with
                | Div -> X86.Reg R.eax
                | Mod -> X86.Reg { reg = R.EDX; size = 4 }
                | _ -> raise (Failure "it is only Div/Mod case"))
            ; size
            }
        ]
      | _ ->
        [ X86.BinCommand { op = Mov; dest = X86.Reg R.eax; src = lhs_final; size }
        ; X86.ZeroCommand { op = X86.Cltd }
        ; X86.UnCommand { op = IDiv; src = rhs_final }
        ; X86.BinCommand
            { op = Mov
            ; dest = d_final
            ; src =
                (match op with
                | Div -> X86.Reg R.eax
                | Mod -> X86.Reg { reg = R.EDX; size = 4 }
                | _ -> raise (Failure "it is only Div/Mod case"))
            ; size
            }
        ]
    in
    right_commands
  (* SHIFTS using ECX*)
  | AS.EfktBinop { op = (ShiftL | ShiftR) as op; dest = d; lhs; rhs } ->
    let size = X86.L in
    let d_final = get_final (d, size) in
    let lhs_final = get_final (lhs, size) in
    let rhs_final = get_final (rhs, size) in
    let right_commands =
      [ X86.BinCommand { op = Mov; dest = X86.get_free size; src = lhs_final; size }
      ; X86.BinCommand
          { op = Mov; dest = X86.Reg { reg = R.ECX; size = 4 }; src = rhs_final; size }
        (* check for the shift >= 32 *)
      ; X86.Cmp
          { lhs = X86.Reg { reg = R.ECX; size = 4 }
          ; rhs = Imm (Int64.of_int_exn 32)
          ; size
          }
      ; X86.Jump { op = Some AS.Jge; label = errLabel }
        (* pre check end *)
        (* check for the shift < 32 *)
      ; X86.Cmp
          { lhs = X86.Reg { reg = R.ECX; size = 4 }
          ; rhs = Imm (Int64.of_int_exn 0)
          ; size
          }
      ; X86.Jump { op = Some AS.Jl; label = errLabel }
      ; X86.BinCommand
          { op = X86.efkt_to_opr op
          ; dest = X86.get_free size
          ; src = X86.Reg { reg = R.ECX; size = 4 }
          ; size
          }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_free size; size }
      ]
    in
    right_commands
  | _ -> failwith "translate_efkt not implemented yet"
;;

let translate_set get_final = function
  | AS.Set { typ; src } ->
    [ X86.Set { op = typ; src = X86.Reg { reg = R.EAX; size = 2 } }
    ; X86.BinCommand
        { op = Movzx
        ; src = X86.Reg { reg = R.EAX; size = 2 }
        ; dest = X86.Reg { reg = R.EAX; size = 4 }
        ; size = X86.L
        }
    ; X86.BinCommand
        { op = Mov
        ; src = X86.Reg { reg = R.EAX; size = 4 }
        ; dest = get_final (src, X86.L)
        ; size = X86.L
        }
    ]
  | _ -> failwith "Not a set operation on translate_set"
;;

(* TODO could have an issue with ordering staff *)
let translate_call
    (get_final : AS.operand * X86.size -> X86.operand)
    (fname : string)
    (stack_args : (Temp.t * AS.size) list)
  =
  let call = [ X86.Call fname ] in
  if List.length stack_args = 0
  then call
  else (
    let jump_int =
      let n = List.length stack_args in
      if n % 2 = 0 then n * 8 else (n * 8) + 8
    in
    let jump_size = Int64.of_int_exn jump_int in
    let arg_moves =
      List.concat_mapi
        ~f:(fun i (t, sz) ->
          let sz = X86.to_size sz in
          let src = get_final (AS.Temp t, sz) in
          let d = X86.Stack (i * 8) in
          match src with
          | Stack mem_i ->
            [ X86.BinCommand
                { op = Mov
                ; dest = X86.get_free sz
                ; src = X86.Stack (mem_i + jump_int)
                ; size = sz
                }
            ; X86.BinCommand { op = Mov; dest = d; src = X86.get_free sz; size = sz }
            ]
          | _ -> [ X86.BinCommand { op = Mov; dest = d; src; size = sz } ])
        stack_args
    in
    [ X86.BinCommand
        { op = X86.Add
        ; dest = X86.Reg { reg = R.RSP; size = 8 }
        ; src = X86.Imm jump_size
        ; size = X86.Q
        }
    ]
    @ call
    @ List.rev arg_moves
    @ [ X86.BinCommand
          { op = X86.Sub
          ; dest = X86.Reg { reg = R.RSP; size = 8 }
          ; src = X86.Imm jump_size
          ; size = X86.Q
          }
      ])
;;

let translate_cmp get_final = function
  | AS.Cmp { rhs; lhs; size } ->
    let size = X86.to_size size in
    let lf = get_final (lhs, size) in
    let rf = get_final (rhs, size) in
    (match lf, rf with
    | X86.Stack _, _ ->
      [ X86.BinCommand { op = Mov; src = lf; dest = X86.get_free size; size }
      ; X86.Cmp { lhs = X86.get_free size; rhs = rf; size }
      ]
    | _, _ -> [ X86.Cmp { lhs = lf; rhs = rf; size } ])
  | _ -> failwith "Not a cmp operation on translate_cmp"
;;

let translate_mov (get_final : AS.operand * X86.size -> X86.operand) = function
  | AS.Mov { dest = d; src = s; size } ->
    let size = X86.to_size size in
    let d_final = get_final (d, size) in
    let src_final = get_final (s, size) in
    (match d_final, src_final with
    | _, Imm _ -> [ X86.BinCommand { op = Mov; dest = d_final; src = src_final; size } ]
    | Stack _, Stack _ ->
      (* mov mem, mem *)
      [ X86.BinCommand { op = Mov; dest = X86.get_free size; src = src_final; size }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_free size; size }
      ]
    | Stack _, _ ->
      (* mov mem, mem *)
      [ X86.BinCommand { op = Mov; dest = X86.get_free size; src = src_final; size }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_free size; size }
      ]
    | _ -> [ X86.BinCommand { op = Mov; dest = d_final; src = src_final; size } ])
  | _ -> failwith "translate_mov is getting not mov"
;;

let translate_movsxd (get_final : AS.operand * X86.size -> X86.operand) = function
  | AS.MovSxd { dest = d; src = s } ->
    let d_final = get_final (d, X86.Q) in
    let src_final = get_final (s, X86.L) in
    (match d_final with
    | Stack _ ->
      (* mov mem, mem *)
      [ X86.Movsxd { dest = X86.get_free X86.Q; src = src_final }
      ; X86.BinCommand
          { op = Mov; dest = d_final; src = X86.get_free X86.Q; size = X86.Q }
      ]
    | _ -> [ X86.Movsxd { dest = d_final; src = src_final } ])
  | _ -> failwith "translate_movsxd is getting not movsxd "
;;

let translate_mov_from (get_final : AS.operand * X86.size -> X86.operand) = function
  | AS.MovFrom { dest = d; src = s; size } ->
    let size = X86.to_size size in
    let d_final = get_final (d, size) in
    let src_final = get_final (s, X86.Q) in
    (match src_final, d_final with
    | Stack _, Stack _ ->
      [ X86.BinCommand
          { op = Mov; dest = X86.get_free X86.Q; src = src_final; size = X86.Q }
      ; X86.MovFrom { dest = X86.get_memfree size; src = X86.get_free X86.Q; size }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_memfree size; size }
      ]
    | Stack _, _ ->
      (* mov (mem) -> sth *)
      [ X86.BinCommand
          { op = Mov; dest = X86.get_free X86.Q; src = src_final; size = X86.Q }
      ; X86.MovFrom { dest = d_final; src = X86.get_free X86.Q; size }
      ]
    | _, Stack _ ->
      (* mov (reg) -> mem *)
      [ X86.BinCommand
          { op = Mov; dest = X86.get_free X86.Q; src = src_final; size = X86.Q }
      ; X86.MovFrom { dest = X86.get_memfree size; src = X86.get_free X86.Q; size }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.get_memfree size; size }
      ]
    | Imm _, _ -> failwith "deref of an imm"
    | _ -> [ X86.MovFrom { dest = d_final; src = src_final; size } ])
  | _ -> failwith "translate_mov_from is getting not mov_from"
;;

let translate_mov_to (get_final : AS.operand * X86.size -> X86.operand) = function
  | AS.MovTo { dest = d; src = s; size } ->
    let size = X86.to_size size in
    let d_final = get_final (d, X86.Q) in
    let src_final = get_final (s, size) in
    (match d_final, src_final with
    | _ ->
      (* mov mem, mem *)
      [ (* X86.BinCommand { op = Mov; dest = X86.get_free size; src = src_final; size } *)
        X86.BinCommand { op = Mov; dest = X86.get_free size; src = src_final; size }
      ; X86.BinCommand
          { op = Mov; dest = X86.get_memfree X86.Q; src = d_final; size = X86.Q }
      ; X86.MovTo { src = X86.get_free size; dest = X86.get_memfree X86.Q; size }
      ]
      (* | Stack _, _ ->
      [ X86.BinCommand
          { op = Mov; dest = X86.get_free X86.Q; src = src_final; size = X86.Q }
      ; X86.MovTo { src = src_final; dest = X86.get_free X86.Q; size }
      ]
    | Imm _, _ -> failwith "deref of an imm"
    | _ -> [ X86.MovTo { dest = d_final; src = src_final; size } ] *))
  | _ -> failwith "translate_mov_from is getting not mov_from"
;;

let translate_line
    (retLabel, errLabel)
    (get_final : AS.operand * X86.size -> X86.operand)
    (prev_lines : X86.instr list)
    (line : AS.instr)
    : X86.instr list
  =
  match line with
  (* Translating move operations *)
  | Mov _ -> List.rev_append (translate_mov get_final line) prev_lines
  (* Translating pure operations *)
  | AS.PureBinop _ -> List.rev_append (translate_pure get_final line) prev_lines
  (* Translating effectful operations *)
  | AS.EfktBinop _ -> List.rev_append (translate_efkt get_final errLabel line) prev_lines
  | Unop { op; dest } ->
    X86.UnCommand { op = X86.unary_to_opr op; src = get_final (dest, X86.L) }
    :: prev_lines
  | Jmp l -> X86.Jump { op = None; label = l } :: prev_lines
  | Cjmp { typ; l } -> X86.Jump { op = Some typ; label = l } :: prev_lines
  | Lab l -> X86.Lbl l :: prev_lines
  | Cmp _ -> List.rev_append (translate_cmp get_final line) prev_lines
  (* Translating comments / directive operations *)
  | AS.Comment d -> Comment d :: prev_lines
  | AS.Directive d -> Directive d :: prev_lines
  | AS.Set s -> List.rev_append (translate_set get_final (AS.Set s)) prev_lines
  | AS.Ret -> [ X86.Ret; X86.Jump { op = None; label = retLabel } ] @ prev_lines
  (* | AS.App _ -> failwith "app is not allowed :(" *)
  | AS.AssertFail -> [ X86.Call "abort@plt" ] @ prev_lines
  | AS.Call { fname; args_overflow = stack_args; _ } ->
    translate_call get_final (Symbol.name fname) stack_args @ prev_lines
  | AS.LoadFromStack _ -> prev_lines
  | AS.MovFrom _ -> List.rev_append (translate_mov_from get_final line) prev_lines
  | AS.MovTo _ -> List.rev_append (translate_mov_to get_final line) prev_lines
  | MovSxd _ -> List.rev_append (translate_movsxd get_final line) prev_lines
;;

let get_error_block errLabel =
  [ X86.Lbl errLabel
  ; X86.Comment "Arithmetic Error Label"
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.EAX; size = 4 }
      ; src = X86.Imm Int64.one
      ; size = X86.L
      }
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.ECX; size = 4 }
      ; src = X86.Imm Int64.zero
      ; size = X86.L
      }
  ; X86.ZeroCommand { op = X86.Cltd }
  ; X86.UnCommand { op = X86.IDiv; src = X86.Reg { reg = R.ECX; size = 4 } }
  ]
;;

let get_memErrLabel_block memErrLabel =
  [ X86.Lbl memErrLabel
  ; X86.Comment "Memory Error Label"
  ; X86.BinCommand
      { op = X86.Mov
      ; dest = X86.Reg { reg = R.EDI; size = 4 }
      ; src = X86.Imm (Int64.of_int_exn 12)
      ; size = X86.L
      }
  ; X86.Call "raise@plt"
  ]
;;

(* maps reg_alloc results to  *)
let get_final (reg_map : Regalloc.reg_or_spill TM.t) ((o, size) : AS.operand * X86.size)
    : X86.operand
  =
  match o with
  | AS.Imm n -> X86.Imm n
  | AS.Reg r -> X86.Reg { reg = Regalloc.asr2renum r; size = X86.of_size size }
  | AS.Temp t ->
    (match TM.find_exn reg_map t with
    | Spl i -> X86.Stack i
    | Reg reg -> X86.Reg { reg; size = X86.of_size size })
;;

let block_instrs (fspace : AS.fspace) : AS.instr list list =
  List.mapi fspace.fdef_blocks ~f:(fun i { block; label = labelbt; _ } ->
      if i = 0
      then []
      else (
        let label =
          match labelbt with
          | Label.BlockLbl b -> b
          | FunName _ -> failwith "not first block has a functionname"
        in
        AS.Lab label :: block))
;;

let translate_function (errLabel : Label.t) (fspace : AS.fspace) : X86.instr list =
  match fspace with
  | { fname; fdef_blocks; args = __args } ->
    (* has to be changed to the global one *)
    let reg_map : Regalloc.reg_or_spill TM.t = alloc fspace in
    let stack_cells = Regalloc.mem_count reg_map in
    let final = get_final reg_map in
    (* gets prologue and epilogue of the function *)
    let b, e, retLabel = Helper.get_function_be (fname, __args) reg_map stack_cells in
    let translated instructions : X86.instr list =
      List.fold instructions ~init:[] ~f:(translate_line (retLabel, errLabel) final)
    in
    let res = List.concat_map ~f:translated (block_instrs fspace) in
    let x = (List.nth_exn fdef_blocks 0).block in
    let first_block_code = List.rev (List.nth_exn (List.map ~f:translated [ x ]) 0) in
    let full_rev = List.rev_append e res in
    b @ first_block_code @ List.rev full_rev
;;

let translate (fs : AS.program) ~mfail =
  let arithErrLabel = Label.create () in
  [ Custom.get_alloc_function mfail ]
  @ [ Custom.get_arrayalloc_function mfail ]
  @ [ get_memErrLabel_block mfail ]
  @ [ get_error_block arithErrLabel ]
  @ List.map ~f:(fun f -> translate_function arithErrLabel f) fs
;;

let speed_up (p : X86.instr list) : X86.instr list =
  List.rev
    (List.fold ~init:[] p ~f:(fun prev i ->
         match prev, i with
         | ( X86.BinCommand ({ op = X86.Mov; _ } as m1) :: _
           , X86.BinCommand ({ op = X86.Mov; _ } as m2) ) ->
           if X86.equal_size m1.size m2.size
              && X86.equal_operand m1.src m2.dest
              && X86.equal_operand m2.src m1.dest
           then X86.Comment (X86.format i) :: prev
           else i :: prev
         | _, _ -> i :: prev))
;;

let speed_up_off = false

(* let pp_fspace l = List.map ~f:(X86.format) l
let pp_program l = List.concat_map ~f:pp_fspace l *)
let get_string_list p : X86.instr list =
  if speed_up_off then List.concat p else speed_up (List.concat p)
;;
