module A = Assem_l4
module T = Tree_l4

val codegen : T.program -> mfl:Label.t -> unsafe:bool -> A.program
val set_lea_off : bool -> unit
val set_llvm_off : bool -> unit
