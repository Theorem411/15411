open Core
module AS = Assem_new
module V = Graph.Vertex

let default_val = true

type color = int [@@deriving compare, equal, sexp]

let templist_to_operand l = List.map ~f:(fun t -> AS.Temp t) l

let get_all_addressable_line instr =
  let all_ops : AS.instr -> AS.operand list = function
    | AS.Mov m -> [ m.dest; m.src ]
    | PureBinop b -> [ b.dest; b.lhs; b.rhs ]
    | EfktBinop b -> [ b.dest; b.lhs; b.rhs ]
    | Unop u -> [ u.dest ]
    | Jmp _
    | Cjmp _
    | Lab _
    | AssertFail
    | AS.Directive _
    | AS.Comment _
    | Ret -> []
    | LoadFromStack t -> templist_to_operand t
    | Cmp (l, r) -> [ l; r ]
    | Set { src; _ } -> [ src; AS.Reg EAX ]
    | Call { args_overflow; _ } -> templist_to_operand args_overflow
  in
  List.filter (all_ops instr) ~f:(fun i ->
      match i with
      | AS.Temp _ | AS.Reg _ -> true
      | _ -> false)
;;

let get_all_nodes instrs =
  List.dedup_and_sort
    ~compare:AS.compare_operand
    (List.concat (List.map instrs ~f:get_all_addressable_line))
;;

let back_coloring_adapter : AS.operand * color -> V.t * color = function
  | AS.Temp t, color -> V.T t, color
  | AS.Reg AS.EAX, color -> V.R AS.EAX, color
  | AS.Reg AS.EDX, color -> V.R AS.EDX, color
  | AS.Reg AS.ECX, color -> V.R AS.ECX, color
  | AS.Reg r, color -> V.R r, color
  | _ -> raise (Failure "Can not happen")
;;

let __coloring_debug (program : AS.instr list) : (V.t * color) list =
  let nodes = get_all_nodes program in
  let max_color = List.length nodes in
  let all_colors : color list = List.range 1 (max_color + 1) in
  let coloring = List.zip_exn nodes all_colors in
  List.map coloring ~f:back_coloring_adapter
;;

(*_ COLORING USING REG ALLOCATOR *)
let __coloring ?(debug_mode_translate = default_val) (program : AS.instr list)
    : (V.t * color) list
  =
  (* if debug_mode_translate
  then __coloring_debug program
  else (
    let graph = Live.mk_graph program in
    (* let graph = Graph.mk_interfere_graph program in *)
    Graph.coloring graph) *)
  if debug_mode_translate then __coloring_debug program else __coloring_debug program
;;

let coloring_adapter : V.t * color -> AS.operand * color = function
  | V.T t, color -> AS.Temp t, color
  | V.R AS.EAX, color -> AS.Reg AS.EAX, color
  | V.R AS.EDX, color -> AS.Reg AS.EDX, color
  | V.R r, color -> AS.Reg r, color
;;

(* | _ -> raise (Failure "Not now, brah (coloring adapter getting not eax or edx)") *)

let __regalloc (l : AS.instr list) : (AS.operand * color) list =
  List.map (__coloring l) ~f:coloring_adapter
;;

let __compare_color = compare_color
let __equal_color = equal_color

let get_free_regs (used_regs : AS.reg list) =
  List.filter X86.all_available_regs ~f:(fun (x : AS.reg) ->
      not (List.mem used_regs x ~equal:AS.equal_reg))
;;

let group_by_colors (colors : (AS.operand * color) list) =
  let sorted = List.sort colors ~compare:(fun (_, a) (_, b) -> __compare_color b a) in
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
  List.filter all_colors ~f:(fun c ->
      not (List.exists used_regs_with_color ~f:(fun (_, uc) -> equal_color c uc)))
;;

let assign_frees (free_regs : AS.reg list) (to_be_assigned : color list)
    : (color * X86.operand) list * int
  =
  let available_len = List.length free_regs in
  let colors_len = List.length to_be_assigned in
  if colors_len > available_len
  then (
    let memcell_count = colors_len - available_len in
    let memcells =
      List.map (List.range 1 (memcell_count + 1)) ~f:(fun i -> X86.Mem (i * 8))
    in
    let free_regs = List.map free_regs ~f:(fun x -> X86.Reg x) in
    List.zip_exn to_be_assigned (free_regs @ memcells), memcell_count)
  else (
    let free_regs = List.map free_regs ~f:(fun x -> X86.Reg x) in
    List.zip_exn to_be_assigned (List.take free_regs colors_len), 0)
;;

let assign_colors (op2col : (AS.operand * color) list) : (color * X86.operand) list * int =
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
  let to_be_assigned : color list = get_unassigned_colors groups used_regs_with_color in
  let rest, mem_cell_count = assign_frees free_regs to_be_assigned in
  let used = List.map used_x86regs_with_color ~f:(fun (r, c) -> c, X86.Reg r) in
  List.append used rest, mem_cell_count
;;

let get_callee_regs (reg_map : X86.operand AS.Map.t) =
  let used_map = AS.Map.filter ~f:(fun o -> X86.is_reg o && X86.callee_saved o) reg_map in
  AS.Map.data used_map
;;

let callee_handle reg_map =
  let callee_regs = get_callee_regs reg_map in
  (* save them into stack *)
  let callee_start =
    List.map callee_regs ~f:(fun r -> X86.UnCommand { op = X86.Pushq; src = r })
  in
  let callee_finish =
    List.map (List.rev callee_regs) ~f:(fun r -> X86.UnCommand { op = X86.Popq; src = r })
  in
  callee_regs, callee_start, callee_finish
;;

let get_reg_map
    (op2col : (AS.operand * color) list)
    (col2operand : (color * X86.operand) list)
    : X86.operand AS.Map.t
  =
  let op_col = AS.Map.of_alist_exn op2col in
  let col_x86op = Int.Map.of_alist_exn col2operand in
  AS.Map.map op_col ~f:(Int.Map.find_exn col_x86op)
;;

let reg_alloc (fspace: AS.fspace) =
  match fspace with
  | { fdef = program; _ } ->  
  let op2col : (AS.operand * color) list = __regalloc program in
  let col2operand, mem_cell_count = assign_colors op2col in
  (* let callee_start, rsp_to_rbp, callee_finish = callee_handle col2operand in *)
  let reg_map = get_reg_map op2col col2operand in
  reg_map, mem_cell_count
;;

let get_max_call_count fdef =
  let only_calls =
    List.filter_map
      ~f:(fun i ->
        match i with
        | AS.Call { args_overflow; _ } -> Some (List.length args_overflow)
        | _ -> None)
      fdef
  in
  Option.value ~default:0 (List.max_elt only_calls ~compare:Int.compare)
;;

let do_arg_moves (reg_map : X86.operand AS.Map.t) (args : AS.operand list) total_size =
  let reg_args, stack_args = List.take args 6, List.drop args 6 in
  let reg_moves =
    let srcs = List.mapi reg_args ~f:(fun i _ -> X86.Reg (AS.arg_i_to_reg i)) in
    let dests = List.map reg_args ~f:(AS.Map.find_exn reg_map) in
    let create d s = X86.BinCommand { op = Mov; dest = d; src = s } in
    List.map2_exn dests srcs ~f:create
  in
  let stack_refs =
    List.concat_mapi stack_args ~f:(fun i t ->
        let d = AS.Map.find_exn reg_map t in
        match d with
        | X86.Reg _ ->
          [ X86.BinCommand
              { op = Mov; dest = d; src = X86.Mem (total_size + 16 + (8 * i)) }
          ]
        | X86.Imm _ -> failwith "dest is Imm"
        | X86.Mem _ ->
          [ X86.BinCommand
              { op = Mov
              ; dest = X86.__FREE_REG
              ; src = X86.Mem (total_size + 16 + (8 * i))
              }
          ; X86.BinCommand { op = Mov; dest = d; src = X86.__FREE_REG }
          ])
  in
  reg_moves @ stack_refs
;;

(* arg[7 + i] <- rsp has some size so recalculate *)

let get_function_be (fname, __args, fdef) reg_map mem_cell_count =
  let args = templist_to_operand __args in
  let mc = get_max_call_count fdef in
  let local_count = mem_cell_count in
  let cee_regs, cee_start, cee_finish = callee_handle reg_map in
  (* let cee_count = List.length cee_regs in  *)
  let n = local_count + mc in
  (*active size of frame (local and arg pushes)*)
  let m = n + List.length cee_regs in
  (* total size of frame (added regs)*)
  let (__sub_count, _) : int * int =
    if m % 2 = 0 then n * 8, mc else (n * 8) + 8, mc + 1
  in
  let sub_count = Int32.of_int_exn __sub_count in
  let total_size = __sub_count + List.length cee_regs in
  let locals = do_arg_moves reg_map args total_size in
  let ret_label = Label.create () in
  (* function labels *)
  let enter =
    [ X86.Directive (sprintf ".globl %s" (Symbol.name fname))
    ; X86.Directive (sprintf ".type\t%s, @function" (Symbol.name fname))
    ; X86.FunName (Symbol.name fname)
    ; X86.UnCommand { op = X86.Pushq; src = X86.Reg AS.RBP }
    ; X86.BinCommand { op = Movq; dest = X86.Reg AS.RBP; src = X86.Reg AS.RSP }
    ]
    @ cee_start
    @ [ X86.BinCommand { op = X86.Subq; dest = X86.Reg AS.RSP; src = X86.Imm sub_count } ]
    @ locals
  in
  let exit =
    [ X86.Comment ("return label of " ^ Symbol.name fname)
    ; X86.Lbl ret_label
    ; X86.BinCommand { op = X86.Addq; dest = X86.Reg AS.RSP; src = X86.Imm sub_count }
    ]
    @ cee_finish
    @ [ X86.UnCommand { op = X86.Popq; src = X86.Reg AS.RBP }; X86.Ret ]
  in
  enter, exit, ret_label
;;
