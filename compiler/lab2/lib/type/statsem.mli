module A = Aste

(*_ raise exception when either type check, init check, or return check failed *)
val static_semantic : A.program -> A.program -> unit
