open Anf

let rec pretty_print_imm = function
  | ImmOperation op -> "(" ^ PrinterAst.pretty_print_op op ^ ")"
  | ImmInt n -> string_of_int n
  | ImmString s -> Printf.sprintf "\"%s\"" s
  | ImmBool b -> string_of_bool b
  | ImmId id -> id
  | ImmList lst -> "[" ^ String.concat ", " (List.map pretty_print_imm lst) ^ "]"
  | ImmTuple lst -> "(" ^ String.concat ", " (List.map pretty_print_imm lst) ^ ")"
  | ImmUnit -> "()"
;;

let rec pretty_print_cexpr = function
  | CApplication (f, arg) ->
    Printf.sprintf "%s(%s)" (pretty_print_cexpr f) (pretty_print_cexpr arg)
  | CIf (cond, then_expr, Some else_expr) ->
    Printf.sprintf
      "if %s then %s else %s"
      (pretty_print_imm cond)
      (pretty_print_aexpr then_expr)
      (pretty_print_aexpr else_expr)
  | CIf (cond, then_expr, None) ->
    Printf.sprintf "if %s then %s" (pretty_print_imm cond) (pretty_print_aexpr then_expr)
  | CConstructList (hd, tl) ->
    Printf.sprintf "%s :: %s" (pretty_print_imm hd) (pretty_print_imm tl)
  | CImm imm -> pretty_print_imm imm

and pretty_print_aexpr = function
  | ALetIn (pat, expr, body) ->
    Printf.sprintf
      "let %s = %s in\n%s"
      (PrinterAst.pretty_print_pattern pat)
      (pretty_print_cexpr expr)
      (pretty_print_aexpr body)
  | ACExpr cexpr -> pretty_print_cexpr cexpr
;;

let pretty_print_single_binding rec_flag (ALet (pat, args, body)) =
  match rec_flag with
  | Ast.Recursive ->
    let args_str = String.concat " " args in
    Printf.sprintf
      "let rec %s %s = %s"
      (PrinterAst.pretty_print_pattern pat)
      args_str
      (pretty_print_aexpr body)
  | _ ->
    let args_str = String.concat " " args in
    Printf.sprintf
      "let %s %s = %s"
      (PrinterAst.pretty_print_pattern pat)
      args_str
      (pretty_print_aexpr body)
;;

let pretty_print_anf_decl = function
  | ADSingleLet (rec_flag, binding) -> pretty_print_single_binding rec_flag binding
  | ADMutualRecDecl bindings ->
    "let rec\n  "
    ^ String.concat
        "\n  and "
        (List.map (pretty_print_single_binding Nonrecursive) bindings)
;;

let pretty_print_anf prog = String.concat "\n\n" (List.map pretty_print_anf_decl prog)
