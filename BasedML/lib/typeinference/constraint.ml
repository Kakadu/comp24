(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
open Ast

module Constraint = struct
  type t = typeName * typeName

  let compare = Stdlib.compare
end

let (create_constr : typeName -> typeName -> Constraint.t) =
  fun t1 t2 -> if t1 < t2 then t1, t2 else t2, t1
;;

module ConstraintSet = struct
  include Set.Make (Constraint)

  let pp ppf m =
    Format.fprintf ppf "@[[@[";
    iter
      (fun (v1, v2) -> Format.fprintf ppf "(%a, %a)@\n" pp_typeName v1 pp_typeName v2)
      m;
    Format.fprintf ppf "@]]@]"
  ;;
end
