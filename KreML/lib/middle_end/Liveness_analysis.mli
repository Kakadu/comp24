open Flambda

type range = { var: string; s: int; e: int }
type analysis_result = (string * range list) list

val analyse_fun : fl_fun -> range list
val analyse_program : flstructure -> analysis_result

val pp : Stdlib.Format.formatter ->  analysis_result -> unit