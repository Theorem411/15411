(* modifies given graph and color and returns a map of temp to another temp *)
val prespill : Graph.new_graph -> int Temp.Map.t * int
