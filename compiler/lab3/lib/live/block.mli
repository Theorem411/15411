module A = Assem_new

type block_label_t =
  | Label of Label.t
  | FunName of Symbol.t

type jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t

type block =
  { label : block_label_t
  ; block : A.instr list
  ; jump : jump_tag_t
  }

type fspace_block =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_block : block list
  }

type block_program = fspace_block list

val block_former : A.program-> block_program