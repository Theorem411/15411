open Core
module IM = Cfg.IM
module LM = Cfg.LM
module LS = Cfg.LS

type t =
  { entry : Label.bt
  ; dt : Label.bt list LM.t
  ; dt_rev : Label.bt LM.t
  }

let intersect (dom : int array) (lidx : int) (ridx : int) : int =
  let rec loop finger1 finger2 =
    if finger1 = finger2
    then finger1
    else (
      let rec loop' src tgt = if src >= tgt then src else loop' dom.(src) tgt in
      let finger1' = loop' finger1 finger2 in
      let finger2' = loop' finger2 finger1 in
      loop finger1' finger2')
  in
  loop lidx ridx
;;

let dominator_tree (cfg : Cfg.t) : t =
  let ({ entry; rev_post_order_i2b; rev_post_order_b2i; _ } : Cfg.t) = cfg in
  let dom = Array.init (IM.length rev_post_order_i2b) ~f:(fun _ -> -1) in
  let entry_idx = LM.find_exn rev_post_order_b2i entry in
  let () = dom.(entry_idx) <- entry_idx in
  let rec loop () =
    let mapf idx old_idom =
      let preds_i = Cfg.preds_i cfg idx in
      let preds = List.filter preds_i ~f:(fun idx -> dom.(idx) > 0) in
      match preds with
      | [] -> false
      | p :: preds' ->
        let new_idom = List.fold preds' ~init:p ~f:(intersect dom) in
        new_idom <> old_idom
    in
    let changed = Array.mapi dom ~f:mapf in
    if Array.fold changed ~init:false ~f:( || ) then loop () else ()
  in
  let () = loop () in
  let edges = Array.mapi dom ~f:(fun idx idom -> Cfg.i2b cfg idom, Cfg.i2b cfg idx) in
  let dt = LM.of_alist_multi (Array.to_list edges) in
  let dt_rev =
    Array.mapi dom ~f:(fun idx idom -> Cfg.i2b cfg idx, Cfg.i2b cfg idom)
    |> Array.to_list
    |> LM.of_alist_exn
  in
  { entry = cfg.entry; dt; dt_rev }
;;

let dominator_tree_post_order ({ entry; dt; _ } : t) : Label.bt list =
  let rec tree_dfs (idom : Label.bt) (visited : LS.t) : Label.bt list * LS.t =
    if LS.mem visited idom
    then [], visited
    else (
      let succ = LM.find_exn dt idom in
      let visited' = LS.add visited idom in
      let rev_post_order, visited'' =
        List.fold succ ~init:([], visited') ~f:(fun (res, vst) u ->
          let res', vst' = tree_dfs u vst in
          res' @ res, vst')
      in
      idom :: rev_post_order, visited'')
  in
  let res, _ = tree_dfs entry LS.empty in
  res
;;

type dom_relation =
  | Same
  | IDom
  | Nope

let is_dominate ({ dt_rev; _ } : t) (b1 : Label.bt) (b2 : Label.bt) : dom_relation =
  (*_ return "is b1 dominating b2?" *)
  let rec search (src : Label.bt) (tgt : Label.bt) : bool =
    (*_ return "is tgt in src's ancestor "*)
    let isrc = LM.find_exn dt_rev src in
    if Label.equal_bt src tgt
    then false (*_ search found *)
    else if Label.equal_bt src isrc
    then true (*_ search ends *)
    else search isrc tgt
  in
  if Label.equal_bt b1 b2
  then Same
  else if search b1 b2
  then Nope
  else if search b2 b1
  then IDom
  else failwith "Impossible!"
;;
