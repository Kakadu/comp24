open AstLib.Pp_ast
open Anf_ast
open Format

let pp_identifier fmt = function
  | Id s -> fprintf fmt "%s" s
  | IdConstraint (s, ty) -> pp_typed fmt pp_print_string (s, ty)
;;

let rec pp_immexpr fmt = function
  | ImmConst c -> pp_const fmt c
  | ImmIdentifier s -> pp_ident fmt s
  | ImmConstraint (e, typ) -> AstLib.Pp_ast.pp_typed fmt pp_immexpr (e, typ)
;;

let rec pp_cexpr fmt = function
  | CApp (CApp (CImmExpr (ImmIdentifier (IdentOfDefinable (IdentOp bin_op))), op1), op2)
    ->
    (* bin op pattern *)
    fprintf fmt "(%a %s %a)" pp_cexpr op1 bin_op pp_cexpr op2
  | CApp
      ( e1
      , CApp
          (CApp (CImmExpr (ImmIdentifier (IdentOfDefinable (IdentOp bin_op))), op1), op2)
      ) -> fprintf fmt "%a (%a %s %a)" pp_cexpr e1 pp_cexpr op1 bin_op pp_cexpr op2
  | CApp (e1, e2) -> fprintf fmt "%a %a" pp_cexpr e1 pp_cexpr e2
  | CIf (e_if, e_th, e_el) ->
    fprintf fmt "if %a then %a else %a" pp_immexpr e_if pp_aexpr e_th pp_aexpr e_el
  | CImmExpr e -> pp_immexpr fmt e

and pp_aexpr fmt = function
  | ALetIn (pat_or_op, cexpr, aexpr) ->
    fprintf fmt "let %a = %a in\n%a" pp_identifier pat_or_op pp_cexpr cexpr pp_aexpr aexpr
  | ACExpr cexpr -> pp_cexpr fmt cexpr
;;

let pp_identifiers fmt pats =
  fprintf fmt " ";
  pp_print_list
    ~pp_sep:(fun fmt _ -> fprintf fmt " ")
    (fun fmt value -> pp_identifier fmt value)
    fmt
    pats
;;

let pp_let_body fmt = function
  | pat_or_op, pats, expr ->
    fprintf fmt "%a%a = %a" pp_identifier pat_or_op pp_identifiers pats pp_aexpr expr
;;

let pp_anf_decl fmt = function
  | ADSingleLet (rec_flag, let_body) ->
    fprintf fmt "let%a %a" pp_rec_flag rec_flag pp_let_body let_body
  | ADMutualRecDecl (rec_flag, let_bodies) ->
    let pp_decls =
      pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "\nand ") pp_let_body
    in
    fprintf fmt "let%a %a" pp_rec_flag rec_flag pp_decls let_bodies
;;

let pp_anf_prog = pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ";;\n") pp_anf_decl
