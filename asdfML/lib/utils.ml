let debug = false


let dbg fmt =
  if debug then
    Format.printf fmt
  else
    Format.ifprintf Format.std_formatter fmt