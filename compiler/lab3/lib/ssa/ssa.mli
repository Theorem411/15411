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
module IH : Hashtbl.S with type key := int
module TH : Hashtbl.S with type key := Temp.t

type phi = AS.operand LM.t TM.t
(*_ t <- \phi(l:t') *)

type phi_use_site_t =
  { which_blc : Label.bt
  ; which_temp : Temp.t
  ; which_pred : Label.bt
  }

type use_site_t =
  | LN of int
  | PhiId of phi_use_site_t

type tuse_sites = use_site_t list

type block =
  { label : Label.bt
  ; lines : int list
  ; jump : AS.assem_jump_tag_t
  ; depth : int
  }


type fspace =
  { fname : Symbol.t
  ; args : (Temp.t * AS.size) list
  ; code : AS.instr IH.t
  ; block_info : block LM.t
  ; phies : phi LT.t
  ; t2use : tuse_sites TH.t
  }

type program = fspace list

val ssa : AS.program -> program
(* val phi_opt : program -> program *)
val de_ssa : program -> AS.program