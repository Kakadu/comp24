open Format

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type typ =
  | TBool
  | TInt
  | TUnit
  | TVar of binder
  | TArrow of typ * typ
(* [@@deriving show { with_path = false }] *)

type err =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of typ * typ
  ]

type scheme = S of binder_set * typ

let bool_typ = TBool
let int_typ = TInt
let unit_typ = TUnit
let var x = TVar x
let arrow l r = TArrow (l, r)

let rec pp_typ ppf = function
  | TBool -> fprintf ppf "bool"
  | TInt -> fprintf ppf "int"
  | TUnit -> fprintf ppf "()"
  | TVar x -> fprintf ppf "%s" @@ "'" ^ Char.escaped (Char.chr (x + 97))
  | TArrow (l, r) ->
    (match l with
     | TArrow (_, _) -> fprintf ppf "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf ppf "%a -> %a" pp_typ l pp_typ r)
;;

let pp_scheme ppf = function
  |S (xs, t) -> fprintf ppf "forall %a . %a" VarSet.pp xs pp_typ t
;;

let print_typ typ = 
  let s = Format.asprintf "%a" pp_typ typ in
  Format.printf "%s\n" s
;;

let pp_err ppf : err -> _ = function
  | `Occurs_check -> Format.fprintf ppf "occurs check failed"
  | `No_variable x -> Format.fprintf ppf "Undefined variable '%s'" x
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_typ l pp_typ r
;;

let print_typ_err e =
  let s = Format.asprintf "%a" pp_err e in
  Format.printf "%s\n" s
;;