open Core
module AS = Assem
module V = Graph.Vertex

(* TODO: ADD SUPPORT FOR CALLEE SAVED REGISTERS: 	%esp, %ebx, %ebp, %r12d, %r13d, %r14d, %r15d *)

type __operand =
  | Temp of Temp.t
  | Reg of AS.reg
[@@deriving equal]

type color = int [@@deriving compare, equal, sexp]

(*_ DEBUGGING STAFF  *)

type random_pair_debug = (color * X86.operand) list [@@deriving sexp]
type another_random_pair_debug = (AS.operand * color) list [@@deriving sexp]

(*_ CREATES A COLORING THAT IS UNIQUE FOR ALL TEMPS *)
(*_ ONLY FOR DEBUGGING PURPOSES
let get_all_addressable_line instr =
  let all_ops : AS.instr -> AS.operand list = function
    | AS.Directive _ | AS.Comment _ -> []
    | AS.Binop b -> [ b.dest; b.lhs; b.rhs ]
    | AS.Mov m -> [ m.dest; m.src ]
  in
  List.filter (all_ops instr) ~f:(fun i ->
      match i with
      | AS.Temp _ | AS.Reg _ -> true
      | _ -> false)
;;  *)

(* let get_all_nodes instrs =
  List.dedup_and_sort
    ~compare:AS.compare_operand
    (List.concat (List.map instrs ~f:get_all_addressable_line))
;; *)

(* let back_coloring_adapter : AS.operand * color -> V.t * color = function
  | AS.Temp t, color -> V.T t, color
  | AS.Reg AS.EAX, color -> V.R AS.EAX, color
  | AS.Reg AS.EDX, color -> V.R AS.EDX, color
  | _ -> raise (Failure "Can not happen")
;; *)


(* let __coloring (program : AS.instr list) : (V.t * color) list =
  let nodes = get_all_nodes program in
  let max_color = List.length nodes in
  let all_colors : color list = List.range 1 (max_color + 1) in
  let coloring = List.zip_exn nodes all_colors in
  List.map coloring ~f:back_coloring_adapter
;; *)

(*_ COLORING USING REG ALLOCATOR *)
let __coloring (program : AS.instr list) : (V.t * color) list =
  let graph = Graph.mk_interfere_graph program in
  Graph.coloring graph
;;


let coloring_adapter : V.t * color -> AS.operand * color = function
  | V.T t, color -> AS.Temp t, color
  | V.R AS.EAX, color -> AS.Reg AS.EAX, color
  | V.R AS.EDX, color -> AS.Reg AS.EDX, color
  | _ -> raise (Failure "Not now, brah (coloring adapter getting not eax or edx)")
;;

let __regalloc (l : AS.instr list) : (AS.operand * color) list =
  List.map (__coloring l) ~f:coloring_adapter
;;

let __compare_color = compare_color
let __equal_color = equal_color

let get_free_regs (used_regs : AS.reg list) =
  List.filter X86.all_available_regs ~f:(fun x ->
      not (List.mem used_regs x ~equal:AS.equal_reg))
;;

let group_by_colors (colors : (AS.operand * color) list) =
  let sorted = List.sort colors ~compare:(fun (_, a) (_, b) -> __compare_color b a) in
  (*_ let () = print_source (sexp_of_string "\n sorted" :: ([sexp_of_another_random_pair_debug sorted])) in   *)
  List.group sorted ~break:(fun (_, a) (_, b) -> not (__equal_color a b))
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
  (*_ let () = print_source (sexp_of_string "\nALL_COL" :: (List.map all_colors ~f:sexp_of_color)) in *)
  List.filter all_colors ~f:(fun c ->
      not (List.exists used_regs_with_color ~f:(fun (_, uc) -> equal_color c uc)))
;;

let assign_frees (free_regs : AS.reg list) (to_be_assigned : color list)
    : (color * X86.operand) list
  =
  let available_len = List.length free_regs in
  let colors_len = List.length to_be_assigned in
  if colors_len > available_len
  then (
    let memcell_count = colors_len - available_len in
    let memcells =
      List.map (List.range 1 (memcell_count + 1)) ~f:(fun i -> X86.Mem i)
    in
    let free_regs = List.map free_regs ~f:(fun x -> X86.X86Reg x) in
    List.zip_exn to_be_assigned (free_regs @ memcells))
  else (
    let free_regs = List.map free_regs ~f:(fun x -> X86.X86Reg x) in
    List.zip_exn to_be_assigned (List.take free_regs colors_len))
;;

let assign_colors (op2col : (AS.operand * color) list) : (color * X86.operand) list =
  let groups = group_by_colors op2col in
  (*_ let () = print_source (sexp_of_string "\n groups len " :: ([sexp_of_int (List.length groups)])) in  *)
  (*_ let () = print_source (sexp_of_string "\n groups" :: (List.map groups ~f:sexp_of_another_random_pair_debug)) in  *)
  (*_ get (Reg, i) list *)
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
  (*_ let _ = List.length groups in *)
  (*_ TODO ADD SUPPORT FOR SPILLING ? *)
  let to_be_assigned : color list = get_unassigned_colors groups used_regs_with_color in
  (*_ let () = print_source (sexp_of_string "\nTO_BE_ASSIGNED len" :: (List.map to_be_assigned ~f:sexp_of_color)) in *)
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
  (* Translating move operations *)
  | Mov { dest = d; src = s } ->
    let d_final = get_reg d in
    let src_final = get_reg s in
    X86.BinCommand { op = Mov; dest = d_final; src = src_final } :: prev_lines
  (* Translating simple operations *)
  | Binop { op = (Add | Sub | Mul) as op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    (match d_final with
    | X86.X86Reg _ ->
      List.rev_append
        [ X86.BinCommand { op = Mov; dest = d_final; src = lhs_final }
        ; X86.BinCommand { op = X86.to_opr op; dest = d_final; src = rhs_final }
        ]
        prev_lines
    | Mem _ ->
      List.rev_append
        [ X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = lhs_final }
        ; X86.BinCommand { op = X86.to_opr op; dest = X86.__FREE_REG; src = rhs_final }
        ; X86.BinCommand { op = Mov; dest = d_final; src = X86.__FREE_REG }
        ]
        prev_lines
    | _ -> raise (Failure "X86.Imm can not be a dest"))
  (* Translating div / mod operations *)
  | Binop { op = (Div | Mod) as op; dest = d; lhs; rhs } ->
    let d_final = get_reg d in
    let lhs_final = get_reg lhs in
    let rhs_final = get_reg rhs in
    let right_commands =
      match rhs_final with
      | X86.Imm (n) ->
        [ X86.BinCommand { op = Mov; dest = X86Reg EAX; src = lhs_final }
        ; X86.BinCommand { op = Mov; dest = X86.__FREE_REG; src = X86.Imm (n) }
        ; X86.Zero { op = CLTD }
        ; X86.UnCommand { op = IDiv; src =  X86.__FREE_REG }
        ; X86.BinCommand
            { op = Mov
            ; dest = d_final
            ; src =
                (match op with
                | Div -> X86Reg EAX
                | Mod -> X86Reg EDX
                | _ -> raise (Failure "it is only Div/Mod case"))
            }
        ]
      | _ ->
        [ X86.BinCommand { op = Mov; dest = X86Reg EAX; src = lhs_final }
        ; X86.Zero { op = CLTD }
        ; X86.UnCommand { op = IDiv; src = rhs_final }
        ; X86.BinCommand
            { op = Mov
            ; dest = d_final
            ; src =
                (match op with
                | Div -> X86Reg EAX
                | Mod -> X86Reg EDX
                | _ -> raise (Failure "it is only Div/Mod case"))
            }
        ]
    in
    List.rev_append right_commands prev_lines
  (* Translating comments / directive operations *)
  | AS.Directive d -> Directive d :: prev_lines
  | AS.Comment d -> Comment d :: prev_lines
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
  (*_ let () = print_source (List.map program ~f:AS.sexp_of_instr) in *)
  let op2col : (AS.operand * color) list = __regalloc program in
  let () =
    CustomDebug.print_with_name "\nColoring of temps" [ sexp_of_another_random_pair_debug op2col ]
  in
  (* let col2operand : (color * X86.operand) list = assign_colors op2col in *)
  let (col2operand : random_pair_debug) = assign_colors op2col in
  let () =
    CustomDebug.print_with_name "\nColoring" [ sexp_of_random_pair_debug col2operand ]
  in
  let translated : X86.instr list =
    List.fold program ~init:[] ~f:(translate_line (get_reg_h (op2col, col2operand)))
  in
  List.rev translated
;;
