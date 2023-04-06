module AS = Assem_l4
module V = Graph.Vertex
module VM = Graph.Vertex.Map
module VT = Graph.VertexTable
module TM = Temp.Map

type color = (int[@deriving compare, equal, hash])

val coalesce : Graph.new_graph -> color VT.t -> (V.t * V.t) list -> V.t TM.t
