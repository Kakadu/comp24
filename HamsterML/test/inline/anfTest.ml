open MatchEliminationTest

let anf_prog (s : string) =
  let open HamsterML.Utils.R in
  let convert prog = run @@ HamsterML.Anf.anf_prog prog in
  s |> match_elim_prog |> convert
;;

let pp_anf_prog (s : string) =
  let open HamsterML.PrinterAnf in
  anf_prog s |> pretty_print_anf |> print_string
;;

let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let rec fac n =
  let anf_1 = n = 1 in
  let anf_5 = n - 1 in
  let anf_4 = fac anf_5 in
  let anf_2 = n * anf_4 in
  let anf_0 = if anf_1 then 1 else anf_2 in
  anf_0
;;
