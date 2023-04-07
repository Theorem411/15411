(* modifies given graph and color and returns a map of temp to another temp *)
val prespill : Graph.new_graph -> Singlepass.t -> int Temp.Map.t * int
