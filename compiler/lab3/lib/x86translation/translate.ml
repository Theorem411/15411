open Core
module AS = Assem_new
module V = Graph.Vertex

(*_ DEBUGGING STAFF  *)

type fspace = X86.instr list
type program = fspace list

(* let debug_mode_translate = true *)

let translate_pure get_reg = function
  | AS.PureBinop { op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    (match d_final with
    | X86.Reg _ ->
      [ X86.BinCommand { op = Mov; dest = d_final; src = lhs_final }
      ; X86.BinCommand { op = X86.pure_to_opr op; dest = d_final; src = rhs_final }
      ]
    | Mem _ ->
      [ X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = lhs_final }
      ; X86.BinCommand { op = X86.pure_to_opr op; dest = X86.__FREE_REG; src = rhs_final }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.__FREE_REG }
      ]
    | _ -> failwith "X86.Imm can not be a dest")
  | _ -> failwith "not a pure operation"
;;

let translate_efkt (get_reg : AS.operand -> X86.operand) (errLabel : Label.t) = function
  (* DIV / MOD *)
  | AS.EfktBinop { op = (Div | Mod) as op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    let right_commands =
      match rhs_final with
      | X86.Imm n ->
        [ X86.BinCommand { op = Mov; dest = Reg EAX; src = lhs_final }
        ; X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = X86.Imm n }
        ; X86.ZeroCommand { op = X86.Cltd }
        ; X86.UnCommand { op = IDiv; src = X86.__FREE_REG }
        ; X86.BinCommand
            { op = Mov
            ; dest = d_final
            ; src =
                (match op with
                | Div -> Reg EAX
                | Mod -> Reg EDX
                | _ -> raise (Failure "it is only Div/Mod case"))
            }
        ]
      | _ ->
        [ X86.BinCommand { op = Mov; dest = Reg EAX; src = lhs_final }
        ; X86.ZeroCommand { op = X86.Cltd }
        ; X86.UnCommand { op = IDiv; src = rhs_final }
        ; X86.BinCommand
            { op = Mov
            ; dest = d_final
            ; src =
                (match op with
                | Div -> Reg EAX
                | Mod -> Reg EDX
                | _ -> raise (Failure "it is only Div/Mod case"))
            }
        ]
    in
    right_commands
  (* SHIFTS using ECX*)
  | AS.EfktBinop { op = (ShiftL | ShiftR) as op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    let right_commands =
      [ X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = lhs_final }
      ; X86.BinCommand { op = Mov; dest = Reg AS.ECX; src = rhs_final }
        (* check for the shift >= 32 *)
      ; X86.Cmp { rhs = Reg AS.ECX; lhs = Imm (Int32.of_int_exn 32) }
      ; X86.Jump { op = Some AS.Jge; label = errLabel }
        (* pre check end *)
        (* check for the shift < 32 *)
      ; X86.Cmp { rhs = Reg AS.ECX; lhs = Imm (Int32.of_int_exn 0) }
      ; X86.Jump { op = Some AS.Jl; label = errLabel }
      ; X86.BinCommand
          { op = X86.efkt_to_opr op; dest = X86.__FREE_REG; src = Reg AS.ECX }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.__FREE_REG }
      ]
    in
    right_commands
  | _ -> failwith "translate_efkt not implemented yet"
;;

let translate_set get_reg = function
  | AS.Set { typ; src } ->
    [ X86.Set { op = typ; src = X86.Reg AS.EAX }
    ; X86.BinCommand { op = Movzx; src = X86.Reg AS.EAX; dest = X86.Reg AS.EAX }
    ; X86.BinCommand { op = Mov; src = X86.Reg AS.EAX; dest = get_reg src }
    ]
  | _ -> failwith "Not a set operation on translate_set"
;;

let translate_call
    (get_reg : AS.operand -> X86.operand)
    (fname : string)
    (stack_args : Temp.t list)
  =
  let call = [ X86.Call fname ] in
  if List.length stack_args = 0
  then call
  else (
    let jump_int = List.length stack_args * 8 in
    let jump_size = Int32.of_int_exn jump_int in
    let arg_moves =
      List.concat_mapi
        ~f:(fun i t ->
          let src = get_reg (AS.Temp t) in
          let d = X86.Mem (i * 8) in
          match src with
          | Mem mem_i ->
            [ X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = X86.Mem (mem_i + jump_int) }
            ; X86.BinCommand { op = Mov; dest = d; src = X86.__FREE_REG }
            ]
          | _ -> [ X86.BinCommand { op = Mov; dest = d; src } ])
        stack_args
    in
    [ X86.BinCommand { op = X86.Addq; dest = X86.Reg AS.RSP; src = X86.Imm jump_size } ]
    @ call
    @ List.rev arg_moves
    @ [ X86.BinCommand { op = X86.Subq; dest = X86.Reg AS.RSP; src = X86.Imm jump_size } ])
;;

let translate_cmp get_reg = function
  | AS.Cmp (r, l) ->
    let lf = get_reg l in
    let rf = get_reg r in
    (match lf, rf with
    | X86.Mem _, _ ->
      [ X86.BinCommand { op = Mov; src = lf; dest = X86.__FREE_REG }
      ; X86.Cmp { lhs = X86.__FREE_REG; rhs = rf }
      ]
    | _, _ -> [ X86.Cmp { lhs = lf; rhs = rf } ])
  | _ -> failwith "Not a cmp operation on translate_cmp"
;;

let translate_line
    (retLabel : Label.t)
    (errLabel : Label.t)
    (get_reg : AS.operand -> X86.operand)
    (prev_lines : X86.instr list)
    (line : AS.instr)
    : X86.instr list
  =
  match line with
  (* Translating move operations *)
  | Mov { dest = d; src = s } ->
    let d_final = get_reg d in
    let src_final = get_reg s in
    (match d_final, src_final with
    | Mem _, Mem _ ->
      (* mov mem, mem *)
      [ X86.BinCommand { op = Mov; dest = d_final; src = X86.__FREE_REG }
      ; X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = src_final }
      ]
      @ prev_lines
    | Mem _, _ ->
      (* mov mem, mem *)
      [ X86.BinCommand { op = Mov; dest = d_final; src = X86.__FREE_REG }
      ; X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = src_final }
      ]
      @ prev_lines
    | _ -> X86.BinCommand { op = Mov; dest = d_final; src = src_final } :: prev_lines)
  (* Translating pure operations *)
  | AS.PureBinop e -> List.rev_append (translate_pure get_reg (AS.PureBinop e)) prev_lines
  (* Translating effectful operations *)
  | AS.EfktBinop e ->
    List.rev_append (translate_efkt get_reg errLabel (AS.EfktBinop e)) prev_lines
  | Unop { op; dest } ->
    X86.UnCommand { op = X86.unary_to_opr op; src = get_reg dest } :: prev_lines
  | Jmp l -> X86.Jump { op = None; label = l } :: prev_lines
  | Cjmp { typ; l } -> X86.Jump { op = Some typ; label = l } :: prev_lines
  | Lab l -> X86.Lbl l :: prev_lines
  | Cmp (r, l) -> List.rev_append (translate_cmp get_reg (AS.Cmp (r, l))) prev_lines
  (* Translating comments / directive operations *)
  | AS.Comment d -> Comment d :: prev_lines
  | AS.Directive d -> Directive d :: prev_lines
  | AS.Set s -> List.rev_append (translate_set get_reg (AS.Set s)) prev_lines
  | AS.Ret -> [ X86.Ret; X86.Jump { op = None; label = retLabel } ] @ prev_lines
  (* | AS.App _ -> failwith "app is not allowed :(" *)
  | AS.AssertFail -> [ X86.Call "abort" ] @ prev_lines
  | AS.Call { fname; args_overflow = stack_args; _ } ->
    translate_call get_reg (Symbol.name fname) stack_args @ prev_lines
  | AS.LoadFromStack _ -> [] @ prev_lines
;;

(* | _ -> failwith "not implemented" *)

(* let mem_handle = function
  | 0 -> raise (Failure "can not happen to have 0 count and mem_handle")
  | cnt ->
    let n =
      match Int32.of_int (cnt * 4) with
      | Some x -> x
      | None -> raise (Failure "Unexpected None")
    in
    ( [ X86.BinCommand { op = X86.Subq; dest = X86.Reg RSP; src = X86.Imm n } ]
    , [ X86.BinCommand { op = X86.Addq; dest = X86.Reg RSP; src = X86.Imm n } ] )
;; *)

let get_error_block errLabel =
  [ X86.Lbl errLabel
  ; X86.BinCommand { op = X86.Mov; dest = X86.Reg AS.EAX; src = X86.Imm Int32.one }
  ; X86.BinCommand { op = X86.Mov; dest = X86.Reg AS.ECX; src = X86.Imm Int32.zero }
  ; X86.ZeroCommand { op = X86.Cltd }
  ; X86.UnCommand { op = X86.IDiv; src = X86.Reg AS.ECX }
  ]
;;

let translate_function errLabel (fspace : AS.fspace) : X86.instr list =
  match fspace with
  | { fname; fdef; args = __args } ->
    (* has to be changed to the global one *)
    let reg_map, mem_cell_count = Helper.reg_alloc fdef in
    let get_reg o =
      match o with
      | AS.Imm n -> X86.Imm n
      | _ -> AS.Map.find_exn reg_map o
    in
    let b, e, retLabel =
      Helper.get_function_be (fname, __args, fdef) reg_map mem_cell_count
    in
    let translated : X86.instr list =
      List.fold fdef ~init:[] ~f:(translate_line retLabel errLabel get_reg)
    in
    let full_rev = List.rev_append e translated in
    b @ List.rev full_rev
;;

let translate fs =
  let errLabel = Label.create () in
  [ get_error_block errLabel ] @ List.map ~f:(fun f -> translate_function errLabel f) fs
;;

(* let pp_fspace l = List.map ~f:(X86.format) l
let pp_program l = List.concat_map ~f:pp_fspace l *)
let get_string_list p : X86.instr list = List.concat p
