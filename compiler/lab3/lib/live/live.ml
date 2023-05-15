open Core
module B = Block
module V = Graph.Vertex
module SP = Singlepass

module T = struct
  type t = Label.bt [@@deriving compare, equal, sexp, hash]
end

module LM = Map.Make (T)
module LT = Hashtbl.Make (T)

type give_to_block_t = V.Set.t LT.t (*_ update by union *)
type block_receive_t = V.Set.t LT.t (*_ update by replace *)

(*_ give_live_to and receive_live_from *)
let give_to_block_init (ls : Label.bt list) =
  LT.of_alist_exn (List.map ls ~f:(fun l -> l, V.Set.empty))
;;

(*_ update by union new with old values *)
let give_to_block_update (giveto : give_to_block_t) (l : Label.bt) (vnew : V.Set.t)
  =
  LT.update giveto l ~f:(fun vsopt ->
      match vsopt with
      | None -> vnew
      | Some vs -> V.Set.union vs vnew)
;;

(*_ update by replacing old values with new ones *)
let block_recieve_init (ls : Label.bt list) =
  LT.of_alist_exn (List.map ls ~f:(fun l -> l, V.Set.empty))
;;

let block_receive_update_and_check
    (recv : block_receive_t)
    (l : Label.bt)
    (vnew : V.Set.t)
  =
  let vold = LT.find_exn recv l in
  let changed = not (V.Set.equal vold vnew) in
  LT.update recv l ~f:(fun _ -> vnew);
  changed
;;

(*_ build predecessor mapping *)
let predecessors ({ fdef_blocks; _ } : B.fspace) =
  let fold_f (ls, acc) (block : B.block) =
    match block.jump with
    | B.JCon { jt; jf } ->
      let res = LM.add_multi ~key:(Label.BlockLbl jt) ~data:block acc in
      let res' = LM.add_multi ~key:(Label.BlockLbl jf) ~data:block res in
      block.label :: ls, res'
    | B.JUncon l -> block.label :: ls, LM.add_multi ~key:(Label.BlockLbl l) ~data:block acc
    | B.JRet -> block.label :: ls, acc
  in
  List.fold fdef_blocks ~init:([], LM.empty) ~f:fold_f
;;

let mk_liveness_fspace (fspace : B.fspace) =
  (*_ determine predecessors of all the blocks *)
  let labs_rev, preds = predecessors fspace in
  (*_ init major hashtbl's *)
  let give_to_block = give_to_block_init labs_rev in
  let block_receive = block_recieve_init labs_rev in
  (* init wq *)
  let wq = Queue.of_list (List.rev fspace.fdef_blocks) in
  (* init final result, which is a SP.t *)
  let singlepass_tbl = SP.init_table fspace in
  let rec wq_loop wq =
    let iter (block : B.block) =
      (*_ all predecessors of block *)
      let pred_block = LM.find_multi preds block.label in
      let pred_label = List.map pred_block ~f:(fun b -> b.label) in
      (*_ singlepass to get new liveout *)
      let livein = LT.find_exn give_to_block block.label in
      let liveout = SP.singlepass singlepass_tbl block livein in
      (*_ update give_to_block of all the predecessors *)
      let giveto_f l = give_to_block_update give_to_block l liveout in
      let () = List.iter pred_label ~f:giveto_f in
      (*_ update block_receive and check if liveout is changed *)
      let () =
        if block_receive_update_and_check block_receive block.label liveout
        then Queue.enqueue_all wq pred_block
        else ()
      in
      wq_loop wq
    in
    match Queue.dequeue wq with
    | None -> ()
    | Some b -> iter b
  in
  wq_loop wq;
  singlepass_tbl
;;

(*_ the final mk_graph_fspace function *)
module VertexTable = Hashtbl.Make (V)

let mk_graph_fspace (fspace : B.fspace) =
  (*_ run liveness algorithm and extracts vertices and edges *)
  let live_table = mk_liveness_fspace fspace in
  let vertices, edges = SP.get_edges_vertices live_table fspace in
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
  V.Map.of_hashtbl_exn graph'
;;