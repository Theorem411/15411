open Core
module L = Label
module LComp : Comparable.S with type t := L.bt
module LS = LComp.Set
module LM = LComp.Map
module LH : Hashtbl.S with type key := L.bt
module IComp : Comparable.S with type t := int
module IM = IComp.Map

type t =
  { entry : L.bt
  ; graph : LS.t LM.t
  ; preds : LS.t LM.t
  ; rev_post_order_i2b : L.bt IM.t
  ; rev_post_order_b2i : int LM.t
  }

type edge_t =
  { pred : L.bt
  ; chld : L.bt
  }

type cfg_input =
  { entry : L.bt
  ; nodes : L.bt list
  ; edges : edge_t list
  }

type cfg_output =
  { cfg : t
  ; dead : L.bt list
  }

val create : cfg_input -> cfg_output
val preds_i : t -> int -> int list
val preds_b : t -> L.bt -> LS.t
val i2b : t -> int -> L.bt
val b2i : t -> L.bt -> int
val pp_edge_t : edge_t -> string
val pp_cfg_input : cfg_input -> string
val pp_graph_hashtbl : L.bt list LH.t -> string 
val pp_graph : LS.t LM.t -> string
val pp_rev_post_order_i2b : L.bt IM.t -> string
val pp_rev_post_order_b2i : int LM.t -> string