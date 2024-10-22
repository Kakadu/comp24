open Types
open Format

let type_id_to_name n =
  let rec helper n acc =
    match n with
    | _ when n < 0 -> invalid_arg "Id must be >= 0"
    | n when n < 26 ->
      let char = Char.chr (n + Char.code 'a') in
      Char.escaped char ^ acc
    | _ ->
      let char = Char.chr ((n mod 26) + Char.code 'a') in
      helper ((n / 26) - 1) (Char.escaped char ^ acc)
  in
  helper n ""
;;

let rec pp_typ fmt = function
  | TVar var -> fprintf fmt "'%s" (type_id_to_name var)
  | TGround x ->
    (match x with
     | TInt -> fprintf fmt "int"
     | TBool -> fprintf fmt "bool"
     | TUnit -> fprintf fmt "()")
  | TArrow (left, right) ->
    (match left with
     | TArrow (_, _) -> fprintf fmt "(%a) -> %a" pp_typ left pp_typ right
     | _ -> fprintf fmt "%a -> %a" pp_typ left pp_typ right)
  | TTuple xs ->
    fprintf fmt "(";
    pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ", ") pp_typ fmt xs;
    fprintf fmt ")"
;;

let pp_error fmt : error -> _ = function
  | `Occurs_check -> fprintf fmt "Occurs check failed"
  | `No_variable s -> fprintf fmt "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf fmt "Unification failed on %a and %a" pp_typ l pp_typ r
  | `TODO s -> fprintf fmt "TODO %s" s
;;

module VarSet = struct
  include Set

  let pp ppf s =
    fprintf ppf "VarSet = [ ";
    Base.Set.iter s (fprintf ppf "%d; ");
    fprintf ppf "]"
  ;;
end

let pp_scheme fmt = function
  | xs, ty -> fprintf fmt "forall %a . %a" VarSet.pp xs pp_typ ty
;;
