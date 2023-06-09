
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
val pp_dom : int array -> string
val pp_dt : Label.bt list LM.t -> string
val pp_dt_rev : Label.bt LM.t -> string