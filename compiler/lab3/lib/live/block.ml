(* open Core
module A = Assem

let format_args args = List.map ~f:Temp.name args |> String.concat ~sep:","

type block_label_t =
  | BlockLabel of Label.t
  | FunName of (Symbol.t * Temp.t list)
[@@deriving compare, sexp, equal, hash]

let format_block_label_t = function
  | BlockLabel l -> sprintf "BlockLbl(%s):" (Label.name l)
  | FunName (f, args) -> sprintf "FunLbl %s(%s):" (Symbol.name f) (format_args args)
;;

type jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t

let format_jump_tag_t = function
  | JRet -> "ret"
  | JUncon l -> "jump -> " ^ Label.name l
  | JCon j -> "jump -> " ^ Label.name j.jt ^ " or " ^ Label.name j.jf
;;

type block =
  { label : block_label_t
  ; block : (int * A.instr) list
  ; jump : jump_tag_t
  }

let format_block f =
  sprintf
    "[\n%s\n|\n  %s|\n%s\n]\n\n"
    (format_block_label_t f.label)
    (List.map ~f:(fun (j, instr) -> Int.to_string j ^ ":" ^ A.format_instr instr) f.block
    |> String.concat ~sep:"  ")
    (format_jump_tag_t f.jump)
;;

type fspace_block =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_block : block list
  }

let format_fspace_block fb =
  sprintf
    "%s(%s){\n%s}\n\n"
    (Symbol.name fb.fname)
    (format_args fb.args)
    (List.map ~f:format_block fb.fdef_block |> String.concat)
;;

type block_program = fspace_block list

let is_jump ((_, f) : int * A.instr) ((_, s) : int * A.instr) : bool =
  match f with
  | A.Ret | A.Jmp _ -> true
  | A.Cjmp _ ->
    (match s with
    | A.Jmp _ -> false
    | _ -> failwith "after cjmp, you do not have jmp")
  | _ -> false
;;

(* given instruction list and label, finds the correct ending / raises an exception *)
let get_ending_with_label (b : (int * A.instr) list) bl =
  let n = List.length b in
  let _, last_jmp_instr = List.nth_exn b (n - 1) in
  (* let () =
    prerr_endline
      (sprintf
         "Doing label %s. n = %d. Last instr = %s"
         (format_block_label_t bl)
         n
         (A.format_instr last_jmp_instr)) 
   in *)
  (* filter out empty labels *)
  if n = 1
  then None
  else (
    let res =
      match last_jmp_instr with
      | A.Ret -> { label = bl; jump = JRet; block = b }
      | A.Jmp goto_l ->
        if n <= 2
        then
          (* do not have to check if block has a conditional jump *)
          { label = bl; jump = JUncon goto_l; block = b }
        else (
          (* have to check if the block has a conditional jump *)
          match List.nth_exn b (n - 2) with
          (* conditional jump *)
          | _, A.Cjmp cjmp ->
            { label = bl; jump = JCon { jt = cjmp.l; jf = goto_l }; block = b }
          (* not conditional *)
          | _ -> { label = bl; jump = JUncon goto_l; block = b })
      | _ -> { label = bl; jump = JRet; block = b }
        (* failwith
          (sprintf
             "the last instruction is not jump: [%s]"
             (A.format_instr last_jmp_instr)) *)
    in
    Some res)
;;

let to_block (b : (int * A.instr) list) : block option =
  let _, label_instr = List.nth_exn b 0 in
  let l =
    match label_instr with
    | A.Lab l -> l
    | _ -> failwith (sprintf "BLOCK\n{%s} is not a label" (A.format_instr label_instr))
  in
  get_ending_with_label b (BlockLabel l)
;;

let of_block (f : A.fspace) : fspace_block =
  (* remove jumps right after returns *)
  let fdef_filtered =
    List.filteri f.fdef ~f:(fun i instr ->
        if i = 0
        then true
        else (
          match List.nth_exn f.fdef (i - 1), instr with
          (* does not filter in only if it is a jump after return or double returns *)
          | A.Ret, A.Jmp _ -> false
          | A.Ret, A.Ret -> false
          | _ -> true))
  in
  let enum_fdef = List.mapi ~f:(fun i instr -> i, instr) fdef_filtered in
  let blocks = List.group ~break:is_jump enum_fdef in
  let first_block =
    get_ending_with_label (List.nth_exn blocks 0) (FunName (f.fname, f.args))
  in
  let rest_blocks = List.map ~f:to_block (List.drop blocks 1) in
  { fname = f.fname; args = f.args; fdef_block = List.filter_opt (first_block :: rest_blocks) }
;;

let blocks_former (funcs : A.fspace list) = List.map ~f:of_block funcs
let pp_fspace (b : fspace_block) : string = format_fspace_block b

let pp_all_blocks (blocks : fspace_block list) : string =
  "---Blocks---\n\n" ^ (List.map ~f:pp_fspace blocks |> String.concat ~sep:";\n\n\n")
;; *)