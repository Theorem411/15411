open Core
module B = Block
module V = Graph.Vertex

include Identifiable.S with type t := Int.t

type ht_entry = {
    test: int
}

val singlepass : _ Hashtbl.t -> B.fspace_block -> V.Set.t -> V.Set.t


val get_edge_vertex: _ Hashtbl.t -> B.fspace_block -> (V.Set.t) * (V.t * V.t list)