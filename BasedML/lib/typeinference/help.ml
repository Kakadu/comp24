(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

module SetString = struct
  include Set.Make (String)

  let pp ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun s -> Format.fprintf ppf "(%s)@\n" s) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type type_form =
  | TFFlat of Ast.type_name
  | TFSchem of SetString.t * Ast.type_name
[@@deriving show { with_path = false }]

let const2type : Ast.constant -> Ast.type_name = function
  | Ast.CBool _ -> Ast.TBool
  | Ast.CInt _ -> Ast.TInt
;;

let rec get_tv_from_tp acc = function
  | Ast.TBool | Ast.TInt -> acc
  | Ast.TPoly x -> SetString.add x acc
  | Ast.TFunction (t1, t2) ->
    SetString.union (get_tv_from_tp acc t1) (get_tv_from_tp SetString.empty t2)
  | Ast.TList t1 -> get_tv_from_tp acc t1
  | Ast.TTuple t_lst -> List.fold_left get_tv_from_tp acc t_lst
;;
