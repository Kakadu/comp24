open Flambda

type range = { var: string; s: int; e: int }

val analyse_fun : fl_fun -> range list