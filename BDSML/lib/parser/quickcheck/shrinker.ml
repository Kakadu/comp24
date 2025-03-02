open QCheck.Iter
open Parser.Ast
open Shrinker_utils

let shrink_typexpr = function
  | Type_tuple l -> of_list l
  | Type_fun l -> of_list l
  | Type_params (t, _) -> return t
  | _ -> empty
;;

let shrink_pattern = function
  | Pat_type (p, t) ->
    let* t' = shrink_typexpr t in
    of_list [ Pat_type (p, t'); p ]
  | Pat_tuple l -> of_list l
  | Pat_or (p1, p2) -> of_list [ p1; p2 ]
  | Pat_construct (_, p) ->
    (match p with
     | Some p -> return p
     | None -> empty)
  | _ -> empty
;;

let rec shrink_let_binding = function
  | Pat_binding (p, e) ->
    let* p' = shrink_pattern p in
    let* e' = shrink_expr e in
    return (Pat_binding (p', e'))
  | Val_binding (id, pl, e) ->
    let* e' = shrink_expr e in
    let* pl' = shrink_list shrink_pattern pl in
    return (Val_binding (id, pl', e'))

and shrink_case c =
  let* p = shrink_pattern c.left in
  let* e = shrink_expr c.right in
  return { left = p; right = e }

and shrink_expr = function
  | Exp_type (e, t) ->
    let* t' = shrink_typexpr t in
    of_list [ Exp_type (e, t'); e ]
  | Exp_let (r, lb_list, e) ->
    let* lb_list' = shrink_list shrink_let_binding lb_list in
    of_list [ Exp_let (r, lb_list', e); e ]
  | Exp_fun (pl, e) ->
    let* pl' = shrink_list shrink_pattern pl in
    of_list [ Exp_fun (pl', e); e ]
  | Exp_function cl ->
    let* cl' = shrink_list shrink_case cl in
    return (Exp_function cl')
  | Exp_apply (e1, e2) -> of_list [ e1; e2 ]
  | Exp_match (e, cl) ->
    let* cl' = shrink_list shrink_case cl in
    of_list [ Exp_match (e, cl'); e ]
  | Exp_tuple el -> of_list el
  | Exp_construct (_, e) ->
    (match e with
     | Some e -> return e
     | None -> empty)
  | Exp_if (e1, e2, e3) ->
    (match e3 with
     | Some e3 -> of_list [ e1; e2; e3 ]
     | None -> of_list [ e1; e2 ])
  | Exp_sequence (e1, e2) -> of_list [ e1; e2 ]
  | _ -> empty
;;

let shrink_structure_item = function
  | Str_eval e ->
    let* e' = shrink_expr e in
    return (Str_eval e')
  | Str_value (r, lb) ->
    let* lb' = shrink_list shrink_let_binding lb in
    return (Str_value (r, lb'))
;;

let shrink_structure sl = shrink_list shrink_structure_item sl
