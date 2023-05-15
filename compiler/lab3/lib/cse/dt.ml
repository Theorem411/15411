open Core
module IM = Cfg.IM
module LM = Cfg.LM
module LS = Cfg.LS
module LH = Cfg.LH

type t =
  { entry : Label.bt
  ; dt : Label.bt list LM.t
  ; dt_rev : Label.bt LM.t
  }

let pp_dom (dom : int array) : string =
  List.mapi (Array.to_list dom) ~f:(fun i idom -> sprintf "idom(%i)=%i" i idom)
  |> String.concat ~sep:", "
;;

let intersect (dom : int array) (lidx : int) (ridx : int) : int =
  let rec loop finger1 finger2 =
    (* let () = prerr_endline (sprintf "--> call loop(%i, %i)?\n" finger1 finger2) in *)
    if finger1 = finger2
    then finger1
    else (
      let rec loop' src tgt =
        (* let () = prerr_endline (sprintf "--> call loop'(%i, %i)?\n" src tgt) in *)
        if src > tgt then loop' dom.(src) tgt else src
      in
      let finger1' = loop' finger1 finger2 in
      let finger2' = loop' finger2 finger1 in
      loop finger1' finger2')
  in
  loop lidx ridx
;;

let pp_dt (dt : Label.bt list LM.t) : string =
  let dt = LM.to_alist dt in
  sprintf
    "{\n%s}\n"
    (List.map dt ~f:(fun (l, ls) ->
       sprintf
         "%s > %s"
         (Label.name_bt l)
         (List.map ls ~f:Label.name_bt |> String.concat ~sep:", "))
    |> String.concat ~sep:"; \n")
;;

let pp_dt_rev (dt_rev : Label.bt LM.t) : string =
  let dt_rev = LM.to_alist dt_rev in
  sprintf
    "idom {%s}"
    (List.map dt_rev ~f:(fun (l, idom) ->
       sprintf "%s:%s" (Label.name_bt l) (Label.name_bt idom))
    |> String.concat ~sep:", ")
;;

let dominator_tree (cfg : Cfg.t) : t =
  let ({ entry; rev_post_order_i2b; rev_post_order_b2i; _ } : Cfg.t) = cfg in
  (* let () =
    prerr_endline
      (sprintf "how is post-order? %s\n" (Cfg.pp_rev_post_order_i2b rev_post_order_i2b))
  in *)
  (*_ init dom *)
  let dom = Array.init (IM.length rev_post_order_i2b) ~f:(fun _ -> -1) in
  let entry_idx = LM.find_exn rev_post_order_b2i entry in
  let () = dom.(entry_idx) <- entry_idx in
  (*_ main loop *)
  let key_range = List.range 0 (Array.length dom) in
  let rec loop () =
    (* let () = prerr_endline (sprintf "restart iteration with dom = %s\n" (pp_dom dom)) in *)
    let fold_f changed idx =
      let old_idom = dom.(idx) in
      let preds_i = Cfg.preds_i cfg idx in
      let preds = List.filter preds_i ~f:(fun idx -> dom.(idx) >= 0) in
      (* let () =
        prerr_endline
          (sprintf
             ">> workable preds of %i = %s;\n"
             idx
             (List.map preds ~f:Int.to_string |> String.concat ~sep:", "))
      in *)
      match preds with
      | [] -> changed || false
      (* | p :: [] -> 
        let new_idom = p in
        let () =
          printf
            ">> old_idom = %i/%s --> new_idom = %i/%s\n"
            old_idom
            (Label.name_bt (Cfg.i2b cfg old_idom))
            new_idom
            (Label.name_bt (Cfg.i2b cfg new_idom))
        in
        let () = if new_idom <> old_idom then dom.(idx) <- new_idom else () in
        changed || (new_idom <> old_idom) *)
      | p :: preds' ->
        let new_idom =
          List.fold preds' ~init:p ~f:(fun new_idom pred -> intersect dom pred new_idom)
        in
        (* let () =
          prerr_endline (sprintf ">> old_idom = %i --> new_idom = %i\n" old_idom new_idom)
        in *)
        let () = if new_idom <> old_idom then dom.(idx) <- new_idom else () in
        changed || new_idom <> old_idom
    in
    let changed = List.fold_left key_range ~init:false ~f:fold_f in
    (* let () = prerr_endline (sprintf "done iteration, changed = %b\n\n" changed) in *)
    if changed then loop () else ()
  in
  let () = loop () in
  (* let () = prerr_endline (sprintf "dom becomes %s\n" (pp_dom dom)) in *)
  (*_ build dt *)
  let edges = Array.mapi dom ~f:(fun idx idom -> Cfg.i2b cfg idom, Cfg.i2b cfg idx) in
  let edges = Array.filter edges ~f:(fun (l1, l2) -> not (Label.equal_bt l1 l2)) in
  let dt = LH.of_alist_multi (Array.to_list edges) in
  let () =
    Array.iteri dom ~f:(fun idx _ ->
      LH.update dt (Cfg.i2b cfg idx) ~f:(fun opt ->
        match opt with
        | None -> []
        | Some lst -> lst))
  in
  let dt = LM.of_hashtbl_exn dt in
  (* let () = prerr_endline (sprintf "dt = %s\n" (pp_dt dt)) in *)
  (*_ build dt_rev *)
  let dt_rev =
    Array.mapi dom ~f:(fun idx idom -> Cfg.i2b cfg idx, Cfg.i2b cfg idom)
    |> Array.to_list
    |> LM.of_alist_exn
  in
  (* let () = prerr_endline (sprintf "dt_rev = %s\n" (pp_dt_rev dt_rev)) in *)
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
