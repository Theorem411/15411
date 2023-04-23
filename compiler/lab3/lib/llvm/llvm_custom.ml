open Core
module SSA = Ssa
module AS = Assem_l4
module Live = Live_faster
module LM = Live.LM
module LS = Live.LS
module TS = Temp.Set

let mac = false
let glob_rm_block_set = ref LS.empty
let add_to_rm_block l = glob_rm_block_set := LS.add !glob_rm_block_set l
let is_rm_block l = Option.is_some (LS.find ~f:(Label.equal_bt l) !glob_rm_block_set)
let glob_boolean = ref TS.empty
let add_to_boolean l = glob_boolean := TS.add !glob_boolean l
let is_bool t = Option.is_some (TS.find ~f:(Temp.equal t) !glob_boolean)
let rem_name = "____JAVAWAY_rem"
let div_name = "____JAVAWAY_div"
let shl_name = "____JAVAWAY_shl"
let shr_name = "____JAVAWAY_shr"

type program = SSA.program

let create p = p
let format_reg r = "%" ^ AS.format_reg r

let format_operand = function
  | AS.Imm n -> Int64.to_string n
  | AS.Temp t -> Temp.name t
  | AS.Reg r -> format_reg r
;;

let format_pure_operation = function
  | AS.Add -> "add"
  | AS.Sub -> "sub"
  | AS.Mul -> "mul"
  | AS.BitAnd -> "and"
  | AS.BitXor -> "xor"
  | AS.BitOr -> "or"
;;

let format_size = function
  | AS.L -> "i32*"
  | AS.S -> "i32"
;;

let format_set_typ = function
  | AS.Sete -> "eq"
  | AS.Setne -> "ne"
  | AS.Setg -> "sgt"
  | AS.Setge -> "sgt"
  | AS.Setl -> "slt"
  | AS.Setle -> "sle"
;;

let format_label (l : Label.t) = "%L" ^ Int.to_string (Label.number l)
let format_label_raw (l : Label.t) = "L" ^ Int.to_string (Label.number l)

let is_bool_cmp (dest, lhs, rhs) =
  let () =
    match dest with
    | AS.Temp d -> add_to_boolean d
    | _ -> failwith "is_bool_cmp dest is not temp"
  in
  match lhs, rhs with
  | AS.Temp l, AS.Temp r -> is_bool l && is_bool r
  | AS.Temp l, _ -> is_bool l
  | _, AS.Temp r -> is_bool r
  | _, _ -> false
;;

let format_instr' : AS.instr -> string = function
  | PureBinop ({ op = AS.BitAnd | AS.BitOr | AS.BitXor; _ } as binop) ->
    sprintf
      "%s = %s %s %s, %s"
      (format_operand binop.dest)
      (format_pure_operation binop.op)
      (format_size binop.size)
      (format_operand binop.lhs)
      (format_operand binop.rhs)
  | PureBinop binop ->
    sprintf
      "%s = %s nsw %s %s, %s"
      (format_operand binop.dest)
      (format_pure_operation binop.op)
      (format_size binop.size)
      (format_operand binop.lhs)
      (format_operand binop.rhs)
  | EfktBinop { op = AS.Mod; dest; lhs; rhs } ->
    sprintf
      "%s = call i32 @%s(i32 %s, i32 %s)"
      (format_operand dest)
      rem_name
      (format_operand lhs)
      (format_operand rhs)
  | EfktBinop { op = AS.Div; dest; lhs; rhs } ->
    sprintf
      "%s = call i32 @%s(i32 %s, i32 %s)"
      (format_operand dest)
      div_name
      (format_operand lhs)
      (format_operand rhs)
  | EfktBinop { op = AS.ShiftL; dest; lhs; rhs } ->
    sprintf
      "%s = call i32 @%s(i32 %s, i32 %s)"
      (format_operand dest)
      shl_name
      (format_operand lhs)
      (format_operand rhs)
  | EfktBinop { op = AS.ShiftR; dest; lhs; rhs } ->
    sprintf
      "%s = call i32 @%s(i32 %s, i32 %s)"
      (format_operand dest)
      shr_name
      (format_operand lhs)
      (format_operand rhs)
  | Unop { dest; src; _ } ->
    sprintf
      "%s = xor %s %s, -1"
      (format_operand dest)
      (format_size AS.S)
      (format_operand src)
  | Mov { dest = AS.Reg _ as dest; src; size } ->
    sprintf "; %s <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | Mov { src = AS.Reg _ as src; dest; size } ->
    sprintf "; %s <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | Mov { dest; src; size } ->
    sprintf "%s <-%s- %s" (format_operand dest) (format_size size) (format_operand src)
  | MovSxd { dest; src } ->
    sprintf "movsxd %s <-- %s" (format_operand dest) (format_operand src)
  | Directive dir -> sprintf "%s" dir
  | Comment comment -> sprintf "/* %s */" comment
  | Jmp l -> "; jump " ^ format_label l
  | Cjmp c ->
    sprintf "; %s %s" (c.typ |> AS.sexp_of_jump_t |> string_of_sexp) (format_label c.l)
  | Lab l -> ".Label " ^ format_label l
  | Ret -> "; ret %EAX"
  | Set c ->
    sprintf "; %s %s" (c.typ |> AS.sexp_of_set_t |> string_of_sexp) (format_operand c.src)
  | Cmp { size; lhs; rhs } ->
    sprintf "; cmp%s %s, %s" (format_size size) (format_operand lhs) (format_operand rhs)
  | AssertFail -> "call __assert_fail"
  | Call { fname; args_in_regs; args_overflow; tail_call } ->
    sprintf
      ";call %s(%s|%s)[tail call - %b]"
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
  | LLVM_Jmp l -> sprintf "br label %s" (format_label l)
  | LLVM_Cmp { dest; lhs; rhs; typ; size } ->
    sprintf
      "%s = icmp %s %s %s, %s"
      (format_operand dest)
      (format_set_typ typ)
      (if is_bool_cmp (dest, lhs, rhs) then "i1" else format_size size)
      (format_operand lhs)
      (format_operand rhs)
  | LLVM_Set { dest; lhs; rhs; typ; size } ->
    sprintf
      "%s = icmp %s %s %s, %s"
      (format_operand dest)
      (format_set_typ typ)
      (if is_bool_cmp (dest, lhs, rhs) then "i1" else format_size size)
      (format_operand lhs)
      (format_operand rhs)
  | LLVM_IF { cond; tl; fl } ->
    sprintf
      "br i1 %s, label %s, label %s"
      (format_operand cond)
      (format_label tl)
      (format_label fl)
  | LLVM_Ret None -> "ret void"
  | LLVM_Ret (Some (src, sz)) -> sprintf "ret %s %s" (format_size sz) (format_operand src)
  | LLVM_Call { dest = Some (dest, sz); args; fname } ->
    sprintf
      "%s = call %s @%s(%s)"
      (format_operand dest)
      (format_size sz)
      (Symbol.name fname)
      (List.map args ~f:(fun (op, s) ->
           sprintf "%s %s" (format_size s) (format_operand op))
      |> String.concat ~sep:", ")
  | LLVM_Call { dest = None; args; fname } ->
    sprintf
      "call void @%s(%s)"
      (Symbol.name fname)
      (List.map args ~f:(fun (op, s) ->
           sprintf "%s %s" (format_size s) (format_operand op))
      |> String.concat ~sep:", ")
;;

(* | _ -> failwith "not implemeted yet" *)

let format_instr (instr : AS.instr) : string = "\t" ^ format_instr' instr

let format_bt (l : Label.bt) =
  match l with
  | FunName _ -> "%entry"
  | BlockLbl l -> format_label l
;;

let pp_phi ({ self; alt_selves } : SSA.phi) : string =
  sprintf
    "\t%s = phi i32 %s"
    (* "\t%s = phi (phi_size) %s" *)
    (Temp.name self)
    (List.filter_map alt_selves ~f:(fun (l, op) ->
         if is_rm_block l
         then None
         else Some (sprintf "[%s, %s]" (format_operand op) (format_bt l)))
    |> String.concat ~sep:", ")
;;

let pp_instr (_ : int) (instr : SSA.instr) : string =
  match instr with
  | SSA.ASInstr instr -> format_instr instr
  | SSA.Phi phi -> pp_phi phi
  | SSA.Nop -> ""
;;

let format_parents ~(cfg_pred : LS.t LM.t) (l : Label.bt) =
  let parent_set = LM.find_exn cfg_pred l in
  sprintf
    "; preds = %s"
    (String.concat
       ~sep:", "
       (List.filter_map (LS.to_list parent_set) ~f:(fun l ->
            if is_rm_block l then None else Some (format_bt l))))
;;

let pp_block
    ?(drop_before = None)
    ~(cfg_pred : LS.t LM.t)
    ({ label; lines; _ } : SSA.block)
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
  let format_parent_blocks = format_parents ~cfg_pred in
  match label with
  | Label.BlockLbl l ->
    let parent_set = LM.find_exn cfg_pred label in
    if LS.length parent_set = 0
    then (
      let () = add_to_rm_block label in
      "")
    else
      sprintf
        "%s:\t\t\t\t\t\t\t\t\t\t\t\t%s\n%s\n"
        (format_label_raw l)
        (format_parent_blocks label)
        (String.concat l2code ~sep:"\n")
    (* (AS.format_jump_tag jump) *)
  | Label.FunName _ -> sprintf "%s\n" (String.concat l2code ~sep:"\n")
;;

(* (AS.format_jump_tag jump) *)

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

(* let format_entry_lbl fname = "entry_" ^ Symbol.name fname ^ ":" *)
let format_entry_lbl _ = "entry:"

let format_ret_size (ret_size : AS.size option) =
  Option.value_map ret_size ~default:"void" ~f:format_size
;;

let pp_fspace ({ fname; code; block_info; cfg_pred; ret_size; _ } as fspace : SSA.fspace)
    : string
  =
  let drop_before, args = get_args fspace in
  sprintf
    "; Function Attrs: norecurse nounwind readnone\n\
     define dso_local %s @%s(%s) #0 {\n\
     %s\n\
     }\n"
    (format_ret_size ret_size)
    (Symbol.name fname)
    (pp_args args)
    (match block_info with
    | first_block :: rest ->
      format_entry_lbl fname
      :: pp_block ~drop_before ~cfg_pred first_block code
      :: List.map rest ~f:(fun b -> pp_block ~cfg_pred b code)
      |> String.concat ~sep:"\n"
    | _ -> failwith "fspace can not be empty")
;;

let format_program (prog : program) : string =
  let prog_string = List.map prog ~f:pp_fspace |> String.concat ~sep:"\n" in
  prog_string
;;

let format_mod () =
  "\n; Safe division function\ndefine i32 @"
  ^ rem_name
  ^ "(i32 %a, i32 %b) {\n\
     entry:\n\
    \  ; Check if b is 0\n\
    \  %is_zero = icmp eq i32 %b, 0\n\n\
    \  ; Check if a is INT_MIN and b is -1\n\
    \  %is_int_min = icmp eq i32 %a, -2147483648\n\
    \  %is_minus_one = icmp eq i32 %b, -1\n\
    \  %is_int_min_div_minus_one = and i1 %is_int_min, %is_minus_one\n\n\
    \  ; Combine the two checks\n\
    \  %invalid_division = or i1 %is_zero, %is_int_min_div_minus_one\n\n\
    \  ; If either check is true, call raise(8)\n\
    \  br i1 %invalid_division, label %call_raise, label %continue\n\n\
     call_raise:\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     continue:\n\
     %result = srem i32 %a, %b\n\
     ret i32 %result\n\
     }"
;;

let format_div () =
  "\n; Safe division function\ndefine i32 @"
  ^ div_name
  ^ "(i32 %a, i32 %b) {\n\
     entry:\n\
    \  ; Check if b is 0\n\
    \  %is_zero = icmp eq i32 %b, 0\n\n\
    \  ; Check if a is INT_MIN and b is -1\n\
    \  %is_int_min = icmp eq i32 %a, -2147483648\n\
    \  %is_minus_one = icmp eq i32 %b, -1\n\
    \  %is_int_min_div_minus_one = and i1 %is_int_min, %is_minus_one\n\n\
    \  ; Combine the two checks\n\
    \  %invalid_division = or i1 %is_zero, %is_int_min_div_minus_one\n\n\
    \  ; If either check is true, call raise(8)\n\
    \  br i1 %invalid_division, label %call_raise, label %continue\n\n\
     call_raise:\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     continue:\n\
    \  %result = sdiv i32 %a, %b\n\
    \  ret i32 %result\n\
     }"
;;

let format_shl () =
  "define i32 @"
  ^ shl_name
  ^ "(i32 %value, i32 %shift_amount) {\n\
    \  %is_negative = icmp slt i32 %shift_amount, 0\n\
    \  %is_large = icmp ugt i32 %shift_amount, 31\n\
    \  %invalid = or i1 %is_negative, %is_large\n\
    \  br i1 %invalid, label %error, label %valid\n\n\
     error:\n\
    \  ; Raise SIGFPE\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     valid:\n\
    \  %result = shl i32 %value, %shift_amount\n\
    \  ret i32 %result\n\
     }"
;;

let format_shr () =
  "define i32 @"
  ^ shr_name
  ^ "(i32 %value, i32 %shift_amount) {\n\
    \  %is_negative = icmp slt i32 %shift_amount, 0\n\
    \  %is_large = icmp ugt i32 %shift_amount, 31\n\
    \  %invalid = or i1 %is_negative, %is_large\n\
    \  br i1 %invalid, label %error, label %valid\n\n\
     error:\n\
    \  ; Raise SIGFPE\n\
    \  call void @raise(i32 8)\n\
    \  unreachable\n\n\
     valid:\n\
    \  %result = shl i32 %value, %shift_amount\n\
    \  ret i32 %result\n\
     }"
;;

let format_pre () =
  [ ""
  ; "declare dso_local i32 @raise(i32) #1"
  ; format_div ()
  ; format_mod ()
  ; format_shl ()
  ; format_shr ()
  ]
  |> String.concat ~sep:"\n"
;;

let get_pre (file : string) : string =
  if mac
  then
    sprintf
      "; ModuleID = '%s'\n\
       target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"\n\
       target triple = \"arm64-apple-macosx12.0.0\"\n\
       %s"
      file
      (format_pre ())
  else
    sprintf
      "; ModuleID = '%s'\n\
       source_filename = \"%s\"\n\
       target datalayout = \
       \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n\
       target triple = \"x86_64-pc-linux-gnu\"\n"
      file
      file
;;

let get_post (_ : string) : string =
  if mac
  then
    "\n\
    \  attributes #1 = { nofree norecurse nosync nounwind readnone ssp uwtable \
     \"frame-pointer\"=\"non-leaf\" \"min-legal-vector-width\"=\"0\" \
     \"no-trapping-math\"=\"true\" \"probe-stack\"=\"__chkstk_darwin\" \
     \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"apple-m1\" \
     \"target-features\"=\"+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz\" \
     }\n\n\
    \  !llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8}\n\
    \  !llvm.ident = !{!9}\n\
    \  \n\
    \  !0 = !{i32 2, !\"SDK Version\", [2 x i32] [i32 12, i32 3]}\n\
    \  !1 = !{i32 1, !\"wchar_size\", i32 4}\n\
    \  !2 = !{i32 1, !\"branch-target-enforcement\", i32 0}\n\
    \  !3 = !{i32 1, !\"sign-return-address\", i32 0}\n\
    \  !4 = !{i32 1, !\"sign-return-address-all\", i32 0}\n\
    \  !5 = !{i32 1, !\"sign-return-address-with-bkey\", i32 0}\n\
    \  !6 = !{i32 7, !\"PIC Level\", i32 2}\n\
    \  !7 = !{i32 7, !\"uwtable\", i32 1}\n\
    \  !8 = !{i32 7, !\"frame-pointer\", i32 1}\n\
    \  !9 = !{!\"Apple clang version 14.0.0 (clang-1400.0.29.102)\"}\n"
  else
    "attributes #0 = { norecurse nounwind readnone uwtable \
     \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \
     \"disable-tail-calls\"=\"false\" \"frame-pointer\"=\"none\" \
     \"less-precise-fpmad\"=\"false\" \"min-legal-vector-width\"=\"0\" \
     \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \
     \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \
     \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \
     \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \
     \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\n\
     attributes #1 = { nounwind }\n\
    \     !llvm.module.flags = !{!0}\n\
     !llvm.ident = !{!1}\n\
     !0 = !{i32 1, !\"wchar_size\", i32 4}\n\
     !1 = !{!\"clang version 10.0.0-4ubuntu1 \"}"
;;
