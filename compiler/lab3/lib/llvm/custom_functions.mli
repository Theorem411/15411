module AS = Assem_l4
val mac : bool
val get_efkt_name : AS.efkt_operation -> string
val format_mod : unit -> string
val format_div : unit -> string
val format_shl : unit -> string
val format_shr : unit -> string
val format_pre : unit -> string
val get_pre : string -> string
val get_post : string -> string
