module AS = Assem_l4

type block_label_t =
  | BlockLabel of Label.t
  | FunName of (Symbol.t * Temp.t list)
  [@@deriving compare, sexp, equal, hash]

  type jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t;;


type block =
  { label : block_label_t
  ; block : (int * AS.instr) list
  ; jump : jump_tag_t
  }

type fspace =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_blocks : block list
  }

type block_program = fspace list

val of_fspace : AS.fspace -> fspace
val blocks_former : AS.program -> block_program
val pp_all_blocks : fspace list -> string
val format_block : block -> string