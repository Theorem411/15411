type disjoint_set
module V = Graph.Vertex
module VT = Graph.VertexTable

type forest = disjoint_set VT.t

val create_forest : unit -> forest
val add_vertex : forest -> V.t -> unit
val find : forest -> V.t -> V.t
val union : forest -> V.t -> V.t -> unit
val union_to : forest -> V.t * V.t -> V.t -> unit
val union_to_x : forest -> x:V.t -> y:V.t -> unit
val get_final_parents : forest -> (V.t * V.t) list