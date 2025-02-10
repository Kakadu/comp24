open Ast
open Parser

let fconst ppf c =
  let fprintf x = Format.fprintf ppf x in
  match c with
  | CString s -> fprintf "\"%s\"" s
  | CInt i -> fprintf "%d" i
  | CBool false -> fprintf "false"
  | CBool true -> fprintf "true"
  | CNil -> fprintf "[]"
  | CUnit -> fprintf "()"
;;

let get_var_name x = if is_idc x.[0] then x else "(" ^ x ^ ")"

let flist pp_elem ppf lst =
  List.iteri
    (fun i pat ->
       if i <> 0 then Format.fprintf ppf ", " else ();
       pp_elem ppf pat)
    lst
;;

let rec fpattern ppf pat =
  let fprintf x = Format.fprintf ppf x in
  match pat with
  | PAny -> fprintf "_"
  | PCons (h_pat, t_pat) -> fprintf "(%a :: %a)" fpattern h_pat fpattern t_pat
  | PVar x -> fprintf "%s" (get_var_name x)
  | PTuple lst -> fprintf "(%a)" (flist fpattern) lst
  | PConst c -> fconst ppf c
  | PConstraint (pat, tp) -> fprintf "(%a : %a)" fpattern pat pp_type_annot tp
;;

let frec_flag ppf = function
  | Rec -> Format.fprintf ppf "rec "
  | Nonrec -> ()
;;

let rec fexpr ppf exp =
  let fprintf x = Format.fprintf ppf x in
  match exp with
  | EConst c -> fconst ppf c
  | EVar s -> fprintf "%s" (get_var_name s)
  | EFun (pat, exp) -> fprintf "(fun %a -> %a)" fpattern pat fexpr exp
  | EApply (exp1, exp2) -> fprintf "(%a %a)" fexpr exp1 fexpr exp2
  | ECons (eh, et) -> fprintf "(%a :: %a)" fexpr eh fexpr et
  | EIf (exp_cond, exp_then, exp_else) ->
    fprintf "(if %a then %a else %a)" fexpr exp_cond fexpr exp_then fexpr exp_else
  | ELet (rec_f, binding_list, exp_body) ->
    fprintf "(let %a " frec_flag rec_f;
    List.iteri
      (fun i binding ->
         if i <> 0 then fprintf " and " else ();
         fbinding ppf binding)
      binding_list;
    fprintf " in %a)" fexpr exp_body
  | ETuple lst -> fprintf "(%a)" (flist fexpr) lst
  | EMatch (exp, pat_exp_lst) ->
    fprintf "(match %a with" fexpr exp;
    List.iter
      (fun (pat, exp) -> fprintf "\n| %a -> %a" fpattern pat fexpr exp)
      pat_exp_lst;
    fprintf ")"

and fbinding ppf ((pat, exp) : binding) =
  Format.fprintf ppf "%a = %a" fpattern pat fexpr exp
;;

let fstr_item ppf decl =
  let fprintf x = Format.fprintf ppf x in
  match decl with
  | SEval expr -> fprintf "%a;;\n" fexpr expr
  | SValue (rec_f, binding_list) ->
    fprintf "let %a" frec_flag rec_f;
    List.iteri
      (fun i binding ->
         if i <> 0 then fprintf " and " else ();
         fbinding ppf binding)
      binding_list;
    fprintf ";;\n"
;;

let fstructure ppf (str : structure) = List.iter (fstr_item ppf) str
let print_structure decls = Format.asprintf "%a" fstructure decls
