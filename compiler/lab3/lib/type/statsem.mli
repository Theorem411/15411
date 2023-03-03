module A = Aste

(*_ raise exception when either type check, init check, or return check failed *)
val static_semantic : hdr:A.program -> src:A.program -> unit
