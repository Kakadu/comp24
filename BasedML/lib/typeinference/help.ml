(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module PolyType = struct
  type t = Ast.poly_type

  let compare = compare
end

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

module MapPolyType = struct
  include Map.Make (PolyType)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%a\": %a@],@\n" Ast.pp_poly_type k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

module SetPolyType = struct
  include Set.Make (PolyType)

  let pp ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun s -> Format.fprintf ppf "(%a)@\n" Ast.pp_poly_type s) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type type_form =
  | TFFlat of Ast.type_name
  | TFSchem of SetPolyType.t * Ast.type_name
[@@deriving show { with_path = false }]

let rec get_tv_from_tp acc = function
  | Ast.TBool | Ast.TInt | Ast.TUnit -> acc
  | Ast.TPoly x -> SetPolyType.add x acc
  | Ast.TFunction (t1, t2) ->
    SetPolyType.union (get_tv_from_tp acc t1) (get_tv_from_tp SetPolyType.empty t2)
  | Ast.TList t1 -> get_tv_from_tp acc t1
  | Ast.TTuple t_lst -> List.fold_left get_tv_from_tp acc t_lst
;;
