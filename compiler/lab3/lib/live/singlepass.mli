module B = Block
module V = Graph.Vertex

type t

(* this creates the initial def and use table *)
val init_table : B.fspace_block -> t

(* do a single pass for the given block and the input. 
   returns the live in of the first line *)
val singlepass : t -> B.block -> V.Set.t -> V.Set.t

(* returns the vertex set and edge list of the given block *)
val get_edges_vertices : t -> B.fspace_block -> V.Set.t * (V.t * V.t) list

(* preprocess block-level defs and uses *)
val block_defs_uses : t -> B.block -> V.Set.t * V.Set.t
