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
module IH : Hashtbl.S with type key := int

type phi =
  { self : Temp.t
  (* ; size : AS.size *)
  ; alt_selves : (Label.bt * AS.operand) list
  }

type instr =
  | ASInstr of AS.instr
  | Phi of phi
  | Nop

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
  }

type program = fspace list

(*_ going into ssa *)
val ssa : AS.program -> program
(*_ going out of ssa *)
val de_ssa : program -> AS.program