open Ast

type ll_expr =
  | LLConstant of const
  | LLIdentifier of id
  | LLIfThenElse of ll_expr * ll_expr * ll_expr
  | LLApplication of ll_expr * ll_expr
  | LLConstraint of ll_expr * type_annot
  | LLTuple of ll_expr list
  | LLMatch of ll_expr * (pattern * ll_expr) list
  | LLLetIn of rec_flag * pattern * ll_expr * ll_expr

type ll_declaration =
  | LLDSingleLet of rec_flag * ll_let
  | LLDMutualRecDecl of rec_flag * ll_let list

and ll_let = LLLet of pattern * pattern list * ll_expr

let rec pp_type_name ppf tp =
  let rec_call tp = pp_type_name ppf tp in
  let fprintf x = Format.fprintf ppf x in
  match tp with
  | AUnit -> fprintf "unit"
  | AInt -> fprintf "int"
  | ABool -> fprintf "bool"
  | AString -> fprintf "string"
  | AVar name -> fprintf "'%s" name
  | ATuple lst ->
    fprintf "(";
    List.iteri
      (fun i tp ->
         if i <> 0 then fprintf " * " else ();
         rec_call tp)
      lst;
    fprintf ")"
  | AFun (tp_arg, tp_ret) -> fprintf "(%a -> %a)" pp_type_name tp_arg pp_type_name tp_ret
  | AList tp -> fprintf "(%a list)" pp_type_name tp
;;

let pp_const ppf c =
  let fprintf x = Format.fprintf ppf x in
  match c with
  | CInt i -> fprintf "%d" i
  | CBool false -> fprintf "false"
  | CBool true -> fprintf "true"
  | CNil -> fprintf "[]"
  | CUnit -> fprintf "()"
  | CString s -> fprintf "%s" s
;;

let rec pp_pattern ppf pat =
  let rec_call = pp_pattern ppf in
  let fprintf x = Format.fprintf ppf x in
  match pat with
  | PAny -> fprintf "_"
  | PCons (h_pat, t_pat) ->
    fprintf "(";
    rec_call h_pat;
    fprintf " :: ";
    rec_call t_pat;
    fprintf ")"
  | PVar x -> fprintf "%s" x
  | PTuple lst ->
    fprintf "(";
    List.iteri
      (fun i pat ->
         if i != 0 then fprintf ", " else ();
         rec_call pat)
      lst;
    fprintf ")"
  | PConst c -> pp_const ppf c
  | PConstraint (pat, tp) ->
    fprintf "(";
    rec_call pat;
    fprintf " : ";
    pp_type_name ppf tp;
    fprintf ")"
;;

let frestore_rec_flag ppf = function
  | Rec -> Format.fprintf ppf "rec"
  | Nonrec -> ()
;;

let rec pp_llexpr ppf exp =
  let rec_call = pp_llexpr ppf in
  let fprintf x = Format.fprintf ppf x in
  match exp with
  | LLConstant c -> pp_const ppf c
  | LLIdentifier s -> fprintf "%s" s
  | LLApplication (exp1, exp2) ->
    fprintf "(";
    rec_call exp1;
    fprintf " ";
    rec_call exp2;
    fprintf ")"
  | LLIfThenElse (exp_cond, exp_then, exp_else) ->
    fprintf "(if (";
    rec_call exp_cond;
    fprintf ") then ";
    rec_call exp_then;
    fprintf " else (";
    rec_call exp_else;
    fprintf "))"
  | LLLetIn (rec_f, pat, exp_val, exp_body) ->
    fprintf "(let ";
    frestore_rec_flag ppf rec_f;
    fprintf " ";
    pp_pattern ppf pat;
    fprintf " = ";
    rec_call exp_val;
    fprintf " in ";
    rec_call exp_body;
    fprintf ")"
  | LLTuple lst ->
    fprintf "(";
    List.iteri
      (fun i exp ->
         if i != 0 then fprintf ", " else ();
         rec_call exp)
      lst;
    fprintf ")"
  | LLMatch (pat_head, pat_exp_lst) ->
    fprintf "(match ";
    pp_llexpr ppf pat_head;
    fprintf " with ";
    List.iter
      (fun (pat, exp) ->
         fprintf "\n| ";
         pp_pattern ppf pat;
         fprintf " -> ";
         rec_call exp)
      pat_exp_lst;
    fprintf ")"
  | LLConstraint (exp, tp) ->
    fprintf "(";
    rec_call exp;
    fprintf " : ";
    pp_type_name ppf tp;
    fprintf ")"
;;

let pp_ll_declaration fmt = function
  | LLDSingleLet (rec_flag, LLLet (pat, patterns, l)) ->
    Format.fprintf
      fmt
      "let %a %a %a = %a"
      frestore_rec_flag
      rec_flag
      pp_pattern
      pat
      (fun fmt -> List.iter (fun pat -> Format.fprintf fmt "%a " pp_pattern pat))
      patterns
      pp_llexpr
      l
  | LLDMutualRecDecl (rec_flag, bindings) ->
    Format.fprintf fmt "let ";
    frestore_rec_flag fmt rec_flag;
    Format.fprintf fmt " ";
    List.iteri
      (fun i binding ->
         if i != 0 then Format.fprintf fmt " and ";
         match binding with
         | LLLet (pat, patterns, exp) ->
           Format.fprintf fmt " ";
           pp_pattern fmt pat;
           (fun fmt -> List.iter (fun pat -> Format.fprintf fmt " %a " pp_pattern pat))
             fmt
             patterns;
           Format.fprintf fmt " = %a " pp_llexpr exp)
      bindings
;;

let pp_llprogram fmt program =
  List.iter
    (fun decl ->
       pp_ll_declaration fmt decl;
       Format.fprintf fmt "\n")
    program
;;

let print_lldecl decl = Stdlib.Format.printf "%a\n" pp_ll_declaration decl
let print_llprog prog = Stdlib.Format.printf "%a\n" pp_llprogram prog