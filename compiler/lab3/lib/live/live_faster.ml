(* open Core
module B = Block
module V = Graph.Vertex
module SP = Singlepass

module T = struct
  type t = B.block_label_t [@@deriving compare, equal, sexp, hash]
end

module LM = Map.Make (T)
module LT = Hashtbl.Make (T)

(*_ For general passing: important data for each blocks *)
type block_def_use = (V.Set.t * V.Set.t) LM.t
type block_in = V.Set.t LT.t
type block_out = V.Set.t LT.t

let block_in_out_init (ls : B.block_label_t list) = 
  LT.of_alist_exn (List.map ls ~f:(fun l -> l, V.Set.empty))
;;

(*_ for each ret block, add it to predecessor of Exit block *)
let predecessors ({ fdef_block; _ } : B.fspace) = failwith "not implemented"
;;
(*_ the general passing algorithm *)
let mk_liveness_fspace (fspace : B.fspace) =
  let singlepass_tbl = SP.init_table fspace in
  (* precompute block_def_use *)
  let fold_f acc (b : B.block) = LM.add_exn acc ~key:b.label ~data:(SP.block_defs_uses singlepass_tbl b) in
  let block_def_use = List.fold fspace.fdef_block ~init:LM.empty ~f:fold_f in
  (*_ *)
  failwith "not implemented"
;;


(*_ the final mk_graph_fspace function *)
module VertexTable = Hashtbl.Make (V)
let mk_graph_fspace (fspace : B.fspace) = failwith "not implemented"
;; *)