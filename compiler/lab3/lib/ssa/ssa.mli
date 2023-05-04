open Core
module AS = Assem_l4
module B = Block
module Live = Live_faster
module LM = Live.LM
module LS = Live.LS
module LT = Live.LT
module TComp : Comparable.S with type t := Temp.t
module TM = TComp.Map
module TS = TComp.Set
module TH : Hashtbl.S with type key := Temp.t
module IComp : Comparable.S with type t := int
module IS = IComp.Set
module IM = IComp.Map
module IH : Hashtbl.S with type key := int

type params = Temp.t TM.t

type jtag =
  | JRet
  | JUncon of
      { l : Label.bt
      ; params : params
      }
  | JCon of
      { lt : Label.bt
      ; tparams : params
      ; lf : Label.bt
      ; fparams : params
      }

type phi =
  { self : Temp.t (* ; size : AS.size *)
  ; alt_selves : (Label.bt * AS.operand) list
  }

type block_ssa =
  { (*_ each block has a matrix of (orig)temp -> (pred) lab to *)
    label : Label.bt
  ; code : AS.instr list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  ; bparams : params
  ; jtag : jtag
  }

type fspace_ssa =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; fdef : block_ssa list
  ; cfg_pred : LS.t LM.t
  ; l2jtag : jtag LM.t
  ; tmp_cnt : int
  }

type program_ssa = fspace_ssa list

type instr =
  | ASInstr of AS.instr
  | Phi of phi
  | Nop [@deriving equal]

type block_phi =
  { label : Label.bt
  ; code : instr list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  }

type fspace_phi =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; fdef : block_phi list
  ; tmp_cnt : int
  }

type program_phi = fspace_phi list

type block =
  { label : Label.bt
  ; lines : int list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  }

type fspace =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; code : instr IH.t
  ; block_info : block list
  ; tuse : IS.t TH.t
  ; tmp_cnt : int
  }

type program = fspace list

(*_ cfg related functions *)
val to_cfg : fspace -> Cfg.cfg_input

(*_ tuse related functions *)
val tuse_remove_ln : IS.t TH.t -> Temp.t -> int -> unit
val tuse_add_ln : IS.t TH.t -> Temp.t -> int -> unit

(*_ ssa step functions *)
val global_rename : AS.program -> program_ssa
val global_phi : program_ssa -> program_phi
val global_lining : program_phi -> program

(*_ going into ssa *)
val ssa : AS.program -> program

(*_ going out of ssa *)
val de_ssa : program -> AS.program

(*_ debug function: will delete later *)
val pp_fspace : fspace -> string
val pp_instr : int -> instr -> string
val pp_program_ssa : program_ssa -> AS.program -> string
val pp_program_phi : program_phi -> program_ssa -> string
val pp_program : program -> string