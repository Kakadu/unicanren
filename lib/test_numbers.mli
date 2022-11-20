val run_optimistically
  :  ?trace_svars:bool
  -> ?trace_uni:bool
  -> ?trace_calls:bool
  -> Lib.goal
  -> Lib.st
  -> Lib.subst list

val build_num : int -> Lib.Term.t
val poso : string * string list * Lib.goal
val gt1o : string * string list * Lib.goal
val full_addero : string * string list * Lib.goal
val addero : string * string list * Lib.goal
val gen_addero : string * string list * Lib.goal
val pluso : string * string list * Lib.goal
val default_env : Lib.st
