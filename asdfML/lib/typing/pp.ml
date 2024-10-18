open Types
open Format

let rec pp_typ fmt = function
  | TVar var -> fprintf fmt "'%c" (Char.chr (var + Char.code 'a'))
  | TGround x ->
    (match x with
     | TInt -> fprintf fmt "int"
     | TBool -> fprintf fmt "bool"
     | TUnit -> fprintf fmt "()")
  | TArrow (left, right) ->
    (match left with
     | TArrow (_, _) -> fprintf fmt "(%a) -> %a" pp_typ left pp_typ right
     | _ -> fprintf fmt "%a -> %a" pp_typ left pp_typ right)
;;

let pp_error fmt : error -> _ = function
  | `Occurs_check -> Format.fprintf fmt "Occurs check failed"
  | `No_variable s -> Format.fprintf fmt "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf fmt "Unification failed on %a and %a" pp_typ l pp_typ r
  | `TODO -> Format.fprintf fmt "TODO"
;;

module VarSet = struct
  include Set

  let pp ppf s =
    Format.fprintf ppf "VarSet = [ ";
    Base.Set.iter s (Format.fprintf ppf "%d; ");
    Format.fprintf ppf "]"
  ;;
end

let pp_scheme fmt = function
  | xs, ty -> fprintf fmt "forall %a . %a" VarSet.pp xs pp_typ ty
;;
