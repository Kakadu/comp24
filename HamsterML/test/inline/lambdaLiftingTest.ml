open HamsterML.LL
open ParserTest

let lambda_lift_prog (s : string) = R.run (ll_prog (parse_prog s))