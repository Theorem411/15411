module AS = Assem_l4
module V = Graph.Vertex
module VM = Graph.Vertex.Map

type color = (int[@deriving compare, equal, hash])

type t =
  { graph : Graph.new_graph
  ; v2c : color VM.t
  ; fspace : AS.fspace
  }

val coalesce : Graph.new_graph -> color VM.t -> AS.fspace -> t
