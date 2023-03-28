module A = Assem

type bt =
  | BlockLbl of Label.t
  | FunName of
      { fname : Symbol.t
      ; args : Temp.t list
      }
      [@@deriving compare, sexp, equal, hash]

type jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t

type block =
  { label : bt
  ; block : (int * A.instr) list
  ; jump : jump_tag_t
  }

type fspace_block =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_block : block list
  }

type block_program = fspace_block list

val blocks_former : A.program -> block_program
val pp_all_blocks : fspace_block list -> string
