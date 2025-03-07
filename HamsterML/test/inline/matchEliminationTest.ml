open HamsterML.ME
open LambdaLiftingTest

let match_elim_prog (s : string) = s |> lambda_lift_prog |> convert_prog

let pp_match_elim_prog (s : string) =
  let open HamsterML.PrinterME in
  match_elim_prog s |> pretty_print_me_prog |> print_string
;;
