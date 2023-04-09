val alloc_fname : unsafe:bool -> string
val alloc_array_fname : unsafe:bool -> string
val get_alloc_function : Label.t -> unsafe:bool -> X86.instr list
val get_arrayalloc_function : Label.t -> unsafe:bool -> X86.instr list
