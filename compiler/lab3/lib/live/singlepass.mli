module B = Block
module V = Graph.Vertex
module AS = Assem_l4
module OperS = AS.Set

type t

(* this creates the initial def and use table *)
val init_table : B.fspace -> t

(* do a single pass for the given block and the input. 
   returns the live in of the first line *)
val singlepass : t -> B.block -> V.Set.t -> V.Set.t

(* returns the vertex set and edge list of the given block *)
val get_edges_vertices : t -> B.fspace -> V.Set.t * (V.t * V.t) list

(* gives back (block uses set, block defs set) *)
val uses_defs_block: B.block -> OperS.t * OperS.t
val get_uses_exn: t -> int -> V.Set.t