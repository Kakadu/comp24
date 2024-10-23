let debug = Sys.getenv_opt "DEBUG" |> Option.value ~default:"false" |> bool_of_string

let dbg fmt =
  if debug then Format.printf fmt else Format.ifprintf Format.std_formatter fmt
;;
