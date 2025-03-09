(* Convert program to ANF *)
val anf_prog : string -> HamsterML.Anf.anf_decl list
(* Convert program to ANF and print *)
val pp_anf_prog : string -> unit
