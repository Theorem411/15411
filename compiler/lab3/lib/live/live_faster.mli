open Core
module B = Block
module SP = Singlepass
module AS = Assem_l4
module V = Graph.Vertex
module L = Label

module LComp : Comparable.S with type t := L.bt
module LM = LComp.Map
module LS = LComp.Set

module OperS = AS.Set

type live_package_t =
  { block_def : OperS.t LM.t
  ; block_use : OperS.t LM.t
  ; block_in : OperS.t LM.t
  ; block_out : OperS.t LM.t
  ; cfg_pred : LS.t LM.t
  }

val mk_liveness_fspace : B.fspace -> live_package_t
val mk_graph_fspace : B.fspace -> Graph.new_graph * SP.t
