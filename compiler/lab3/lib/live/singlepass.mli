open Core
module B = Block
module V = Graph.Vertex

type ht_entry = {
    test: int
}

val singlepass : (int, ht_entry) Hashtbl.t -> B.block -> V.Set.t -> V.Set.t

val get_edge_vertex: (int, ht_entry) Hashtbl.t -> B.block -> (V.Set.t) * (V.t * V.t list)