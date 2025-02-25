open Llast
open Ast
open PprinterAST

let rec pp_llexpression formatter e =
  match e with
  | LLConst c -> pp_constant formatter c
  | LLVar n -> Format.fprintf formatter "%s" n
  | LLApp (e1, e2) ->
    Format.fprintf formatter "(%a %a)" pp_llexpression e1 pp_llexpression e2
  | LLIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      pp_llexpression
      i
      pp_llexpression
      t
      pp_llexpression
      e
  | LLTuple t -> Format.fprintf formatter "(%a)" (pp_tuple pp_llexpression) t
  | LLEbinOp (op, l, r) ->
    Format.fprintf formatter "(%a %a %a)" pp_llexpression l pp_binop op pp_llexpression r
  | LLVars (head, tail) ->
    let rec print_list formatter = function
      | LLList (h, t) -> Format.fprintf formatter "%a; %a" pp_llexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_llexpression tail
    in
    Format.fprintf formatter "%a" print_list (LLList (head, tail))
  | LLList (head, tail) ->
    let rec print_list formatter = function
      | LLConst CNil -> Format.fprintf formatter ""
      | LLList (h, t) -> Format.fprintf formatter "%a; %a" pp_llexpression h print_list t
      | _ -> Format.fprintf formatter "%a" pp_llexpression tail
    in
    Format.fprintf formatter "[%a]" print_list (LLList (head, tail))
  | LLMatch (m, p) ->
    Format.fprintf formatter "(match %a with" pp_llexpression m;
    List.iter
      (fun (p, lle) ->
        Format.fprintf formatter "\n| %a -> %a" pp_pattern p pp_llexpression lle)
      p;
    Format.fprintf formatter ")"
    | LLLetIn (n, args, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %s %a = %a\n  in %a"
      (String.concat ", " n)
      (fun fmt -> List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
      args
      pp_llexpression
      e
      pp_llexpression
      ine
;;

let pp_llbindings formatter bindings =
  List.iter
    (fun bind ->
      match bind with
      | LLLet bindings_list ->
        (* Перебираем все let-объявления внутри LLLet *)
        List.iteri
          (fun i (r, n_list, p, e) ->
            let n_str = String.concat ", " n_list in
            if i = 0
            then
              (* Первое объявление начинается с "let" *)
              Format.fprintf
                formatter
                "let %a %s %a = %a"
                pp_rec
                r
                n_str
                (fun fmt ->
                  List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
                p
                pp_llexpression
                e
            else
              (* Все последующие объявления начинаются с "and" *)
              Format.fprintf
                formatter
                "and %a %s %a = %a"
                pp_rec
                r
                n_str
                (fun fmt ->
                  List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
                p
                pp_llexpression
                e)
          bindings_list;
        Format.fprintf formatter "\n"
      | LLExpression e ->
        pp_llexpression formatter e;
        Format.fprintf formatter "\n")
    bindings
;;

let pp_lambda_lifting bindings = Format.asprintf "%a" pp_llbindings bindings
