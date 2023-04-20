open Core
module SSA = Ssa
module AS = Assem_l4

(* TODO *)
(* 
  1. Do sth with sizes of temps, we need them at least for phi functions 
     (may be create a hash table on the trans level and when ssa-ing also use 
     that table to look-up and update it along the way)
  2. Change how return works, especially with size or sth ...
     Add a size of return functions, now we can not just translate things into  
  3. Change how Cmp behaves, may be we have to create some llvm specific 
     commands that are ignored on the orginary paths
  4. Change how call works


*)

type program = SSA.program

let create p = p
let format_reg r = "%" ^ AS.format_reg r

let format_operand = function
  | AS.Imm n -> "$" ^ Int64.to_string n
  | AS.Temp t -> Temp.name t
  | AS.Reg r -> format_reg r
;;

let format_pure_operation = function
  | AS.Add -> "add"
  | AS.Sub -> "-"
  | AS.Mul -> "*"
  | AS.BitAnd -> "&"
  | AS.BitXor -> "^"
  | AS.BitOr -> "|"
;;

let format_efkt_operation = function
  | AS.Div -> "/"
  | AS.Mod -> "%"
  | AS.ShiftL -> "<<"
  | AS.ShiftR -> ">>"
;;

let format_unop = function
  | AS.BitNot -> "~"
;;

let format_size = function
  | AS.L -> "i32*"
  | AS.S -> "i32"
;;

let format_instr' : AS.instr -> string = function
  | PureBinop binop ->
    sprintf
      "%s = %s nsw %s %s, %s"
      (format_operand binop.dest)
      (format_pure_operation binop.op)
      (format_size binop.size)
      (format_operand binop.lhs)
      (format_operand binop.rhs)
  | EfktBinop binop ->
    sprintf
      "%s <-- %s %s %s"
      (format_operand binop.dest)
      (format_operand binop.lhs)
      (format_efkt_operation binop.op)
      (format_operand binop.rhs)
  | Unop { dest; op; src } ->
    sprintf "%s <-s- %s%s" (format_operand dest) (format_unop op) (format_operand src)
  | Mov { dest; src; size } ->
    sprintf "%s <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | MovSxd { dest; src } ->
    sprintf "movsxd %s <-- %s" (format_operand dest) (format_operand src)
  | Directive dir -> sprintf "%s" dir
  | Comment comment -> sprintf "/* %s */" comment
  | Jmp l -> "jump" ^ Label.name l
  | Cjmp c ->
    sprintf "%s %s" (c.typ |> AS.sexp_of_jump_t |> string_of_sexp) (Label.name c.l)
  | Lab l -> ".Label " ^ Label.name l
  | Ret -> "ret [ret-size] %EAX"
  | Set c ->
    sprintf "%s %s" (c.typ |> AS.sexp_of_set_t |> string_of_sexp) (format_operand c.src)
  | Cmp { size; lhs; rhs } ->
    sprintf "cmp%s %s, %s" (format_size size) (format_operand lhs) (format_operand rhs)
  | AssertFail -> "call __assert_fail"
  | Call { fname; args_in_regs; args_overflow; tail_call } ->
    sprintf
      "call %s(%s|%s)[tail call - %b]"
      (Symbol.name fname)
      (List.map args_in_regs ~f:(fun (r, s) ->
           sprintf "%s%s" (format_reg r) (format_size s))
      |> String.concat ~sep:", ")
      (List.map args_overflow ~f:(fun (op, s) ->
           sprintf "%s%s" (format_operand op) (format_size s))
      |> String.concat ~sep:", ")
      tail_call
  | LoadFromStack ts ->
    sprintf
      "loadfromstack {%s}"
      (List.map ts ~f:(fun (t, s) -> sprintf "%s%s" (Temp.name t) (format_size s))
      |> String.concat ~sep:", ")
  | MovFrom { dest; size; src } ->
    sprintf "%s <-%s- (%s)" (format_operand dest) (format_size size) (format_operand src)
  | MovTo { dest; size; src } ->
    sprintf "(%s) <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | LeaPointer { dest; base; offset; size } ->
    sprintf
      "%s <- lea: [%s] %s + %d"
      (format_operand dest)
      (format_size size)
      (format_operand base)
      offset
  | LeaArray { dest; base; offset; index; scale } ->
    sprintf
      "%s <- lea: %s + %s * %d + %d"
      (format_operand dest)
      (format_operand base)
      (format_operand index)
      scale
      offset
;;

let format_instr (instr : AS.instr) : string = "\t" ^ format_instr' instr

let pp_phi ({ self; alt_selves } : SSA.phi) : string =
  sprintf
    "%s <-- @(%s)"
    (Temp.name self)
    (List.map alt_selves ~f:(fun (l, op) ->
         sprintf "%s:%s" (Label.name_bt l) (AS.format_operand op))
    |> String.concat ~sep:", ")
;;

let pp_instr (l : int) (instr : SSA.instr) : string =
  match instr with
  | SSA.ASInstr instr -> format_instr instr
  | SSA.Phi phi -> Int.to_string l ^ " : " ^ pp_phi phi
  | SSA.Nop -> ""
;;

let pp_block
    ?(drop_before = None)
    ({ label; lines; jump; _ } : SSA.block)
    (code : SSA.instr SSA.IH.t)
    : string
  =
  let l2code =
    List.filter_map lines ~f:(fun l ->
        (* drop everything before and load from stack (including) *)
        if l <= Option.value ~default:(-1) drop_before
        then None
        else (
          let instr = SSA.IH.find_exn code l in
          match instr with
          | Nop -> None
          | _ -> Some (pp_instr l instr)))
  in
  match label with
  | Label.BlockLbl l ->
    sprintf
      "%s:\n%s\n-------------- jump:%s -----------------\n"
      (Label.name l)
      (String.concat l2code ~sep:"\n")
      (AS.format_jump_tag jump)
  | Label.FunName _ ->
    sprintf
      "%s\n-------------- jump:%s -----------------\n"
      (String.concat l2code ~sep:"\n")
      (AS.format_jump_tag jump)
;;

let pp_args (args : (Temp.t * AS.size) list) : string =
  List.map args ~f:(fun (t, sz) -> sprintf "%s %s" (format_size sz) (Temp.name t))
  |> String.concat ~sep:", "
;;

(* to be changed *)
let get_args ({ code; block_info; _ } : SSA.fspace) =
  let first_block = List.nth_exn block_info 0 in
  let load_from_stack_args, load_line =
    List.find_map_exn first_block.lines ~f:(fun l ->
        match SSA.IH.find_exn code l with
        | ASInstr (AS.LoadFromStack tmp_args) -> Some (tmp_args, l)
        | _ -> None)
  in
  let reg_move_lines =
    List.take_while first_block.lines ~f:(fun l ->
        match SSA.IH.find_exn code l with
        | ASInstr (AS.Mov _) -> true
        | _ -> false)
  in
  let reg_args =
    List.filter_map reg_move_lines ~f:(fun l ->
        match SSA.IH.find_exn code l with
        | ASInstr (AS.Mov { dest = AS.Temp t; size; _ }) -> Some (t, size)
        | _ -> failwith "not a reg move in reg_move_args")
  in
  Some load_line, reg_args @ load_from_stack_args
;;

let format_entry_lbl fname = "entry_" ^ Symbol.name fname ^ ":"

let pp_fspace ({ fname; code; block_info; _ } as fspace : SSA.fspace) : string =
  let drop_before, args = get_args fspace in
  sprintf
    "def [ret-size] @%s(%s) {\n%s\n}\n"
    (Symbol.name fname)
    (pp_args args)
    (match block_info with
    | first_block :: rest ->
      format_entry_lbl fname
      :: pp_block ~drop_before first_block code
      :: List.map rest ~f:(fun b -> pp_block b code)
      |> String.concat ~sep:"\n"
    | _ -> failwith "fspace can not be empty")
;;

let format_program (prog : program) : string =
  List.map prog ~f:pp_fspace |> String.concat ~sep:"\n\n"
;;
