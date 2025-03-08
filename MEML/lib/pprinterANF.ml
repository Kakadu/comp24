open Anfast
open PprinterAST

let pp_aconst formatter = function
  | AInt i -> Format.fprintf formatter "%d" i
  | ABool false -> Format.fprintf formatter "false"
  | ABool true -> Format.fprintf formatter "true"
  | ANil -> Format.fprintf formatter "[]"
;;

let rec pp_aexpression formatter e =
  match e with
  | AConst c -> pp_aconst formatter c
  | AVar n -> Format.fprintf formatter "%s" n
  | AApp (e1, e2) ->
    Format.fprintf formatter "(%a %a)" pp_aexpression e1 pp_aexpression e2
  | AIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      pp_aexpression
      i
      pp_aexpression
      t
      pp_aexpression
      e
  | ATuple t -> Format.fprintf formatter "(%a)" (pp_tuple pp_aexpression) t
  | ABinOp (op, l, r) ->
    Format.fprintf formatter "(%a %a %a)" pp_aexpression l pp_binop op pp_aexpression r
  | AVars (head, tail) ->
    let rec print_list formatter = function
      | AList (h, t) -> Format.fprintf formatter "%a; %a" pp_aexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_aexpression tail
    in
    Format.fprintf formatter "%a" print_list (AList (head, tail))
  | AList (head, tail) ->
    let rec print_list formatter = function
      | AConst ANil -> Format.fprintf formatter ""
      | AList (h, t) -> Format.fprintf formatter "%a; %a" pp_aexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_aexpression tail
    in
    Format.fprintf formatter "[%a]" print_list (AList (head, tail))
  | APatLetIn (names, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a = %a\n  in %a"
      pp_pattern
      names
      pp_aexpression
      e
      pp_aexpression
      ine
;;

let pp_abindings formatter bindings =

  List.iter
    (fun bind ->
       match bind with
       | ALet (r, n_list, p, e) ->
         Format.fprintf
           formatter
           "let %a %s %s = %a"
           pp_rec
           r
           n_list
           (String.concat " " p)
           pp_aexpression
           e;
         Format.fprintf formatter "\n"
       | ALetPat (n_list, e) ->
         Format.fprintf formatter "let %a = %a" pp_pattern n_list pp_aexpression e;
         Format.fprintf formatter "\n"
       | AExpression e ->
         pp_aexpression formatter e;
         Format.fprintf formatter "\n")
    bindings
;;

let pp_anf bindings = Format.asprintf "%a" pp_abindings bindings
