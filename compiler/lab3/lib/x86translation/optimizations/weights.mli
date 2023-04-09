module V = Graph.Vertex

val calc_weights : Singlepass.t -> Block.fspace -> (V.t -> int option) * (V.t * V.t) list

