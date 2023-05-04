
module LM = Cfg.LM

type t =
  { entry : Label.bt
  ; dt : Label.bt list LM.t
  ; dt_rev : Label.bt LM.t
  }

val dominator_tree : Cfg.t -> t
val dominator_tree_post_order : t -> Label.bt list

type dom_relation =
  | Same
  | IDom
  | Nope

val is_dominate : t -> Label.bt -> Label.bt -> dom_relation