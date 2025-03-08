open Ty

let pp_typ ppf typ names_mapping =
  let rec helper ppf = function
    | TVar n ->
      (match names_mapping with
       | Some names ->
         (try Format.fprintf ppf "%s" (Base.Map.find_exn names n) with
          | Base.Not_found_s _ | Stdlib.Not_found ->
            Format.fprintf ppf "Names for types are counted incorrectly")
       | None -> Format.fprintf ppf "%i" n)
    | TPrim s -> Format.fprintf ppf "%s" s
    | TArrow (l, r) ->
      (match l with
       | TArrow _ -> Format.fprintf ppf "(%a) -> %a" helper l helper r
       | _ -> Format.fprintf ppf "%a -> %a" helper l helper r)
    | TTuple (h :: list) ->
      let print_item item fmt =
        match item with
        | TArrow _ | TTuple _ -> Format.fprintf ppf (fmt ^^ "(%a)") helper item
        | _ -> Format.fprintf ppf (fmt ^^ "%a") helper item
      in
      List.fold_left (fun _ item -> print_item item " * ") (print_item h "") list
    | TList t ->
      (match t with
       | TArrow _ | TTuple _ -> Format.fprintf ppf "(%a) list" helper t
       | _ -> Format.fprintf ppf "%a list" helper t)
    | TTuple [] -> Format.fprintf ppf ""
  in
  helper ppf typ
;;

let get_type_var_names typ =
  let calc_name letter num =
    "\'"
    ^
    if num = 0
    then Base.Char.to_string letter
    else Base.Char.to_string letter ^ Int.to_string num
  in
  let next_letter c = Base.Char.of_int_exn (Base.Char.to_int c + 1) in
  let next letter num =
    if Char.equal letter 'z' then 'a', num + 1 else next_letter letter, num
  in
  let rec helper names letter num = function
    | TVar n ->
      (match Base.Map.add names ~key:n ~data:(calc_name letter num) with
       | `Ok new_names -> new_names, next letter num
       | `Duplicate -> names, (letter, num))
    | TArrow (l, r) ->
      let names, (letter, num) = helper names letter num l in
      helper names letter num r
    | TList t -> helper names letter num t
    | TTuple (h :: list) ->
      List.fold_left
        (fun (names, (letter, num)) -> helper names letter num)
        (names, (letter, num))
        (h :: list)
    | _ -> names, (letter, num)
  in
  let names, (_, _) = helper (Base.Map.empty (module Base.Int)) 'a' 0 typ in
  names
;;

let pp_typ ppf t =
  let names = get_type_var_names t in
  pp_typ ppf t (Some names)
;;

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | `Occurs_check -> Format.fprintf ppf "Error: Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Error: Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "Error: Unification failed on %a and %a" pp_typ l pp_typ r
  | `Not_solo_var -> fprintf ppf "Error: Simple name expected"
  | `Bad_let -> fprintf ppf "Error: Incorrect structure for creating let"
;;
