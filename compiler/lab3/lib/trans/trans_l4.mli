module A = Asts
module T = Tree_l4

val translate : A.program -> T.program
val set_unsafe : bool -> unit
