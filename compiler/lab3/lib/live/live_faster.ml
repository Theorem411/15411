open Core
module B = Block
module V = Graph.Vertex
module SP = Singlepass
module L = Label

module T = struct
  type t = L.bt [@@deriving compare, equal, sexp, hash]
end

module LM = Map.Make (T)
module LS = Set.Make (T)
module LT = Hashtbl.Make (T)

(*_ For general passing: important data for each blocks *)
type block_du_t =
  { bdef : V.Set.t
  ; buse : V.Set.t
  }

type block_def_use = block_du_t LM.t

type block_lv_t =
  { liveout : V.Set.t (*_ update by union with liveouts of successors *)
  ; livein : V.Set.t (*_ update by replace *)
  }

type block_in_out = block_lv_t LT.t
type cfg_pred = LS.t LM.t
type lab_to_block = B.block LM.t

(*_ many init functions *)
let lab_to_block_init (fspace : B.fspace) : lab_to_block =
  let lbs = List.map fspace.fdef_blocks ~f:(fun b -> b.label, b) in
  LM.of_alist_exn lbs
;;

let wq_init ({ fdef_blocks; _ } : B.fspace) = 
  let blst = List.rev fdef_blocks in
  let blst = List.filter blst ~f:(fun b -> match b.jump with | B.JRet -> true | _ -> false) in
  (* let () = prerr_endline (sprintf "wqinit:%s\n" (List.map blst ~f:(fun b -> Label.name_bt b.label) |> String.concat ~sep:",\n")) in  *)
  let res = Queue.of_list blst in
  res

let cfg_pred_init ({ fdef_blocks; _ } : B.fspace) : cfg_pred =
  let mapf ({ label; jump; _ } : B.block) =
    match jump with
    | B.JCon { jt; jf } -> [ L.bt jt, label; L.bt jf, label ]
    | B.JUncon l -> [ L.bt l, label ]
    | _ -> []
  in
  let l2p = List.map fdef_blocks ~f:mapf |> List.concat in
  LM.map ~f:LS.of_list (LM.of_alist_multi l2p)
;;

let block_def_use_init (fspace : B.fspace) : block_def_use =
  let mapf (b : B.block) =
    let buse, bdef = SP.uses_defs_block b in
    b.label, { buse; bdef }
  in
  let l2du = List.map fspace.fdef_blocks ~f:mapf in
  LM.of_alist_exn l2du
;;

let block_in_out_init ({ fdef_blocks; _ } : B.fspace) : block_in_out =
  let block_lv_init () = { liveout = V.Set.empty; livein = V.Set.empty } in
  let mapf ({ label; _ } : B.block) = label, block_lv_init () in
  let l2i = List.map fdef_blocks ~f:mapf in
  LT.of_alist_exn l2i
;;

(* let rm_dead_blocks (fspace : B.fspace) : B.fspace * cfg_pred =
  let cfg = cfg_pred_init fspace in
  let dlabs = LM.filter cfg ~f:(fun ls -> LS.is_empty ls) |> LM.keys |> LS.of_list in
  let cfg_remain = LM.filter cfg ~f:(fun ls -> not (LS.is_empty ls)) in
  let fdef_blocks' =
    List.filter fspace.fdef_blocks ~f:(fun { label; _ } -> not (LS.mem dlabs label))
  in
  { fspace with fdef_blocks = fdef_blocks' }, cfg_remain
;; *)

(*_ the general passing algorithm *)
let mk_liveness_fspace (fspace : B.fspace) =
  (* init:
     1. init wq : the reverse of the original block ordering 
     2. init cfg : predecessor map 
     3. init l2du : label to block-level def and use 
     4. init l2io : label table from label to active livein and liveout 
     5. init SP.t : table from line to final livein & liveout info *)
  let wq = wq_init fspace in
  let cfg = cfg_pred_init fspace in
  let l2du = block_def_use_init fspace in
  let l2io = block_in_out_init fspace in
  let l2b = lab_to_block_init fspace in
  let sptbl = SP.init_table fspace in
  (*_ the general passing algorithm 
    input: a freshly dequeued block 
    to-do: 
    0. keep the old version of livein(b)
    1. update current block's livein.
      livein'(b) = useb + (liveout(b) - defb)
    2. update all predecessor's liveout. For b' in Pred(b)
      liveout(b') += livein(b)
    3. check if livein(b) == livein'(b). If not, add all predecessors back to wq
     *)
  let general_passing (block : B.block) : unit =
    (*_ load info into variables *)
    let ({ label; _ } : B.block) = block in
    let pred =
      match LM.find cfg label with
      | Some ps -> ps
      | None -> LS.empty
    in
    let { bdef; buse } = LM.find_exn l2du label in
    let { liveout; livein } = LT.find_exn l2io label in
    (*_ update own livein *)
    let livein' = V.Set.union buse (V.Set.diff liveout bdef) in
    let () = LT.update l2io label ~f:(fun _ -> { livein = livein'; liveout }) in
    (*_ update pred's liveout *)
    let update_opt = function
      | Some { liveout; livein } -> { liveout = V.Set.union liveout livein'; livein }
      | None -> failwith "live: not possible"
    in
    let iterf (label : Label.bt) = LT.update l2io label ~f:update_opt in
    let () = LS.iter pred ~f:iterf in
    (*_ check livein convergence and put pred back on wq *)
    let pred_blocks = List.map (LS.to_list pred) ~f:(fun l -> LM.find_exn l2b l) in
    if V.Set.equal livein livein' then () else Queue.enqueue_all wq pred_blocks
  in
  let rec loop (_ : unit) : unit =
    match Queue.dequeue wq with
    | Some block ->
      (* printf
        "right before processing bid=^%s, |wq| = %i\n"
        (Label.name_bt block.label)
        (Queue.length wq); *)
      general_passing block;
      (* prerr_endline
        (sprintf "after processing bid=^%s, |wq| = %i"
        (Label.name_bt block.label)
        (Queue.length wq)); *)
      loop ()
    | None -> ()
  in
  let () = loop () in
  (* let () = prerr_endline "surprise! loop finishes!\n" in *)
  (*_ do one final single passing using biot *)
  let l2io' =
    LT.to_alist l2io |> List.map ~f:(fun (l, lio) -> LM.find_exn l2b l, lio.liveout)
  in
  (* let () = prerr_endline (sprintf "about to go into slow zone! |l2io'| = %i\n" (List.length l2io')) in *)
  let mapf ((b, liveout) : B.block * V.Set.t) = SP.singlepass sptbl b liveout in
  let (_ : V.Set.t list) = List.map l2io' ~f:mapf in
  (* let () = prerr_endline "surprise! SP finishes!\n" in *)
  sptbl
;;

(*_ the final mk_graph_fspace function *)
module VertexTable = Hashtbl.Make (V)

let mk_graph_fspace (fspace : B.fspace) =
  let spt = mk_liveness_fspace fspace in
  let () = prerr_endline "starts making graph!\n" in
  let vertices, edges = SP.get_edges_vertices spt fspace in
  (*_ create a hashtable graph *)
  let graph' = VertexTable.create () in
  let () =
    V.Set.iter vertices ~f:(fun v -> VertexTable.set graph' ~key:v ~data:V.Set.empty)
  in
  let () =
    List.iter edges ~f:(fun (a, b) ->
      let a_set = VertexTable.find_exn graph' a in
      let b_set = VertexTable.find_exn graph' b in
      if not (V.equal a b)
      then (
        let () = VertexTable.set graph' ~key:b ~data:(V.Set.add b_set a) in
        let () = VertexTable.set graph' ~key:a ~data:(V.Set.add a_set b) in
        ())
      else ())
  in
  let res = V.Map.of_hashtbl_exn graph' in
  let () = prerr_endline "done!" in
  res
;;
