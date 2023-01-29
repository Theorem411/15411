open Core
module AS = Assem
module V = Vertex
type __operand =
  | Temp of Temp.t
  | Reg of AS.reg
[@@deriving equal]

type color = int * int [@@deriving compare, equal]

let __coloring (_ : AS.instr list) : (V.t * color) list = []

let coloring_adapter: ((V.t * color) -> (AS.operand * color)) = function
| (V.T t, color) -> (AS.Temp t, color)
| (V.R V.EAX, color)  -> (AS.Reg AS.EAX, color) 
| (V.R V.EDX, color)  -> (AS.Reg AS.EDX, color) 


let __regalloc (l : AS.instr list) : (AS.operand * color) list = List.map (__coloring l) ~f:coloring_adapter
let __compare_color = compare_color

let get_free_regs (used_regs : AS.reg list) =
  let all_available_regs =
    [ AS.EAX
    ; AS.EDX
    ; AS.ECX
    ; AS.ESI
    ; AS.EDI
    ; AS.EBX
    ; AS.R8D
    ; AS.R9D
    ; AS.R10D
    ; AS.R12D
    ; AS.R13D
    ; AS.R14D
    ; AS.R15D
    ]
  in
  List.filter all_available_regs ~f:(fun x ->
      not (List.mem used_regs x ~equal:AS.equal_reg))
;;

let group_by_colors (colors : (AS.operand * color) list) =
  List.sort_and_group colors ~compare:(fun (_, a) (_, b) -> __compare_color a b)
;;

let get_unassigned_colors groups used_regs_with_color =
  let all_colors =
    List.map
      (groups : (AS.operand * color) list list)
      ~f:(fun l ->
        match l with
        | (_, color) :: _ -> color
        | [] -> raise (Failure "Group can not be empty"))
  in
  List.filter all_colors ~f:(fun c ->
      not (List.exists used_regs_with_color ~f:(fun (_, uc) -> equal_color c uc)))
;;

let assign_frees (free_regs : AS.reg list) (to_be_assigned : color list)
    : (color * X86.operand) list
  =
  let available_len = List.length free_regs in
  let colors_len = List.length to_be_assigned in
  if colors_len > available_len
  then
    let memcell_count = colors_len - available_len in
    let memcells = List.map (List.range 1 memcell_count) ~f:(fun i -> X86.Mem (4 * i)) in
    let free_regs = List.map free_regs ~f:(fun x -> X86.X86Reg x) in
    List.zip_exn to_be_assigned (free_regs @ memcells)
  else (
    let free_regs = List.map free_regs ~f:(fun x -> X86.X86Reg x) in
    List.zip_exn to_be_assigned (List.take free_regs colors_len))
;;

let assign_colors (op2col : (AS.operand * color) list) : (color * X86.operand) list =
  let groups = group_by_colors op2col in
  (* get (Reg, i) list *)
  let used_regs_with_color =
    List.filter op2col ~f:(fun l ->
        match l with
        | Reg _, _ -> true
        | _ -> false)
  in
  let used_x86regs_with_color =
    List.map used_regs_with_color ~f:(fun (o, c) ->
        match o with
        | Reg x -> x, c
        | _ -> raise (Failure "non reg in used_regs_with_color"))
  in
  let free_regs : AS.reg list =
    get_free_regs (List.map used_x86regs_with_color ~f:(fun (r, _) -> r))
  in
  let _ = List.length groups in
  (* TODO ADD SUPPORT FOR SPILLING ? *)
  let to_be_assigned : color list = get_unassigned_colors groups used_regs_with_color in
  let rest : (color * X86.operand) list = assign_frees free_regs to_be_assigned in
  let used = List.map used_x86regs_with_color ~f:(fun (r, c) -> c, X86.X86Reg r) in
  List.append used rest
;;

let translate_line
    (get_reg : AS.operand -> X86.operand)
    (prev_lines : X86.instr list)
    (line : AS.instr)
    : X86.instr list
  =
  match line with
  (* Translating simple operations *)
  | Binop { op = (Add | Sub | Mul) as op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    (match d_final with
    | X86.X86Reg _ ->
      [ X86.BinCommand { op = Mov; dest = d_final; src = lhs_final }
      ; X86.BinCommand { op = X86.to_opr op; dest = d_final; src = rhs_final }
      ]
      @ prev_lines
    | Mem _ ->
      [ X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = lhs_final }
      ; X86.BinCommand { op = X86.to_opr op; dest = X86.__FREE_REG; src = rhs_final }
      ; X86.BinCommand { op = Mov; dest = d_final; src = X86.__FREE_REG }
      ]
      @ prev_lines
    | _ -> raise (Failure "imm can not be dest"))
  | Binop { op = (Div | Mod) as op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    [ BinCommand { op = Mov; dest = X86Reg EAX; src = lhs_final }
    ; Zero { op = CLTD }
    ; UnCommand { op = IDiv; src = rhs_final }
    ; BinCommand
        { op = Mov
        ; dest = d_final
        ; src =
            (match op with
            | Div -> X86Reg EAX
            | Mod -> X86Reg EDX
            | _ -> raise (Failure "it is only Div/Mod case"))
        }
    ]
  | AS.Directive d -> Directive d :: prev_lines
  | AS.Comment d -> Comment d :: prev_lines
  | _ -> prev_lines
;;

let get_color_of_operand op2col (o : AS.operand) =
  (fun (_, c) -> c) (List.find_exn op2col ~f:(fun (o2, _) -> AS.equal_operand o o2))
;;

let get_reg_of_color col2operand (c : color) =
  (fun (_, r) -> r) (List.find_exn col2operand ~f:(fun (uc, _) -> equal_color uc c))
;;

let get_reg_h (op2col, col2operand) o =
  match o with
  | AS.Imm n -> X86.Imm n
  | _ -> get_reg_of_color col2operand (get_color_of_operand op2col o)
;;

let translate (program : AS.instr list) : X86.instr list =
  let op2col : (AS.operand * color) list = __regalloc program in
  let col2operand : (color * X86.operand) list = assign_colors op2col in
  let translated : X86.instr list =
    List.fold program ~init:[] ~f:(translate_line (get_reg_h (op2col, col2operand)))
  in
  List.rev translated
;;
