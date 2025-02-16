open Llast
open Ast
open Printer

let rec llexpression_printer formatter e =
  match e with
  | LLConst c -> constant_printer formatter c
  | LLVar n -> Format.fprintf formatter "%s" n
  | LLApp (e1, e2) ->
    Format.fprintf formatter "(%a %a)" llexpression_printer e1 llexpression_printer e2
  | LLIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      llexpression_printer
      i
      llexpression_printer
      t
      llexpression_printer
      e
  | LLLetIn (r, n, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a %s = %a\n  in %a"
      rec_printer
      r
      (String.concat ", " n)
      llexpression_printer
      e
      llexpression_printer
      ine
  | LLTuple t -> Format.fprintf formatter "(%a)" (tuple_printer llexpression_printer) t
  | LLEbinOp (op, l, r) ->
    Format.fprintf
      formatter
      "(%a %a %a)"
      llexpression_printer
      l
      binop_printer
      op
      llexpression_printer
      r
  | LLList (head, tail) ->
    (* Рекурсивно выводим список *)
    let rec print_list formatter = function
      | LLConst CNil -> Format.fprintf formatter ""
      | LLList (h, t) ->
        Format.fprintf formatter "%a; %a" llexpression_printer h print_list t
      | _ -> Format.fprintf formatter "%a" llexpression_printer tail
    in
    Format.fprintf formatter "[%a]" print_list (LLList (head, tail))
  | LLMatch (m, p) ->
    Format.fprintf formatter "(match %a with" llexpression_printer m;
    List.iter
      (fun (pat, exp) ->
        Format.fprintf
          formatter
          "\n| %a -> %a"
          pattern_printer
          pat
          llexpression_printer
          exp)
      p
;;

let llbindings_printer formatter bindings =
  List.iter
    (fun bind ->
      (match bind with
       | LLLet (r, n_list, p, e) ->
         let n_str = String.concat " " n_list in (* Объединяем список строк в одну строку *)
         Format.fprintf
           formatter
           "let %a %s %a = %a"
           rec_printer
           r
           n_str (* Используем преобразованный идентификатор *)
           (fun fmt ->
             List.iter (fun pat -> Format.fprintf fmt "%a " pattern_printer pat))
           p
           llexpression_printer
           e
       | LLExpression e -> llexpression_printer formatter e);
      Format.fprintf formatter "\n")
    bindings
;;

let llprinter bindings = Format.asprintf "%a" llbindings_printer bindings
