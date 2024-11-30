open Ast


module Varset = struct
  include Stdlib.Set.Make(Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"

end

type scheme = S of Varset.t * typ
