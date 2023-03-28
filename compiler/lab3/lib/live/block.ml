open Core
module AS = Assem_l4

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

type block =
  { label : block_label_t
  ; block : (int * AS.instr) list
  ; jump : jump_tag_t
  }

let format_jump_tag = function
  | JRet -> "ret"
  | JUncon l -> "jump -> " ^ Label.name l
  | JCon j -> "jump -> " ^ Label.name j.jt ^ " or " ^ Label.name j.jf
;;

let format_block f =
  sprintf
    "[\n%s\n|\n  %s|\n%s\n]\n\n"
    (format_block_label_t f.label)
    (List.map ~f:(fun (j, instr) -> Int.to_string j ^ ":" ^ AS.format_instr instr) f.block
    |> String.concat ~sep:"  ")
    (format_jump_tag f.jump)
;;

type fspace =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_blocks : block list
  }

let format_fspace fb =
  sprintf
    "%s(%s){\n%s}\n\n"
    (Symbol.name fb.fname)
    (format_args fb.args)
    (List.map ~f:format_block fb.fdef_blocks |> String.concat)
;;

type block_program = fspace list

let global_cnt = ref 0

(* type block = {
  label : block_label_t;
  block : (int * A.instr) list;
  jump : A.jump_tag_t;
} *)
(* numerate the lines globally *)

let numerate (raw : AS.instr list) : (int * AS.instr) list =
  let start = !global_cnt in
  let res = List.mapi ~f:(fun i instr -> start + i, instr) raw in
  global_cnt := start + List.length raw;
  res
;;

let to_bjump = function
  | AS.JRet -> JRet
  | AS.JCon { jt; jf } -> JCon { jt; jf }
  | AS.JUncon l -> JUncon l
;;

let of_block ({ label; block; jump } : AS.block) : block =
  { label = BlockLabel label; jump = to_bjump jump; block = numerate block }
;;

let of_fspace ({ fname; args; fdef_blocks } as f : AS.fspace) : fspace =
  let b = List.nth_exn f.fdef_blocks 0 in
  let args = List.map ~f:(fun (t, _) -> t) args in
  let first_block =
    { label = FunName (f.fname, args); jump = to_bjump b.jump; block = numerate b.block }
  in
  { fname
  ; args
  ; fdef_blocks = first_block :: List.map ~f:of_block (List.drop fdef_blocks 1)
  }
;;

let blocks_former (funcs : AS.fspace list) = List.map ~f:of_fspace funcs
let pp_fspace (b : fspace) : string = format_fspace b

let pp_all_blocks (blocks : fspace list) : string =
  "---Blocks---\n\n" ^ (List.map ~f:pp_fspace blocks |> String.concat ~sep:";\n\n\n")
;;