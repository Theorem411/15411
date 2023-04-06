module V = Graph.Vertex
module VM = V.Map
module TM = Temp.Map
module G = Graph
module VT = Graph.VertexTable

(* given graph and some kind of information, returns map of temp -> i, n. 
   Where the map is temporary to stack index (integer) and 
   n is the number of different indicies applied
*)
let prespill (_ : G.new_graph) = TM.of_alist_exn [], 0
