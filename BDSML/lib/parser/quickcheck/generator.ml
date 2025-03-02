open Parser.Ast
open QCheck.Gen
open Generator_utils

let gen_constant =
  oneof
    [ (small_int >|= fun x -> Const_int x)
    ; (printable >|= fun c -> Const_char c)
    ; (small_string >|= fun s -> Const_string s)
    ]
;;

let rec gen_typexpr = function
  | 0 -> gen_ident >|= fun s -> Type_single s
  | depth ->
    oneof
      [ (let* l = gen_list_helper gen_typexpr depth in
         return (Type_fun l))
      ; (let* l = gen_list_helper gen_typexpr depth in
         return (Type_tuple l))
      ; (let* t = gen_typexpr (depth / 2) in
         let* id = gen_ident in
         return (Type_params (t, id)))
      ]
;;

let gen_rec = oneof [ return Recursive; return Nonrecursive ]

let rec gen_pattern = function
  | 0 ->
    oneof
      [ return Pat_any
      ; (gen_ident >|= fun s -> Pat_var s)
      ; (gen_constant >|= fun c -> Pat_constant c)
      ]
  | depth ->
    oneof
      [ (let* p = gen_pattern (depth / 2) in
         let* t = gen_typexpr (depth / 2) in
         return (Pat_type (p, t)))
      ; (let* l = gen_list_helper gen_pattern depth in
         return (Pat_tuple l))
      ; (let* p1 = gen_pattern (depth / 2) in
         let* p2 = gen_pattern (depth / 2) in
         return (Pat_or (p1, p2)))
      ; gen_construct
          gen_pattern
          (fun (st, p) -> Pat_construct (st, p))
          depth
          (fun p -> Pat_tuple p)
      ]
;;

let rec gen_expr = function
  | 0 ->
    oneof
      [ (gen_ident >|= fun s -> Exp_ident s); (gen_constant >|= fun c -> Exp_constant c) ]
  | depth ->
    oneof
      [ (let* e = gen_expr (depth / 2) in
         let* type_depth = int_range 0 5 in
         let* t = gen_typexpr type_depth in
         return (Exp_type (e, t)))
      ; (let* lb_list = gen_list_helper gen_let_binding depth in
         let* r = gen_rec in
         let* e = gen_expr (depth / 2) in
         return (Exp_let (r, lb_list, e)))
      ; (let* pl = gen_list_helper gen_pattern (depth / 2) in
         let* e = gen_expr (depth / 2) in
         return (Exp_fun (pl, e)))
      ; (let* cl = gen_list_helper gen_case depth in
         return (Exp_function cl))
      ; (let* e1 = gen_expr (depth / 2) in
         let* e2 = gen_expr (depth / 2) in
         return (Exp_apply (e1, e2)))
      ; (let* e = gen_expr (depth / 2) in
         let* cl = gen_list_helper gen_case (depth / 2) in
         return (Exp_match (e, cl)))
      ; (let* el = gen_list_helper gen_expr depth in
         return (Exp_tuple el))
      ; gen_construct
          gen_expr
          (fun (st, e) -> Exp_construct (st, e))
          depth
          (fun e -> Exp_tuple e)
      ; (let* e1 = gen_expr (depth / 3) in
         let* e2 = gen_expr (depth / 3) in
         let* e3 = opt ~ratio:0.5 (gen_expr (depth / 3)) in
         return (Exp_if (e1, e2, e3)))
      ; (let* e1 = gen_expr (depth / 2) in
         let* e2 = gen_expr (depth / 2) in
         return (Exp_sequence (e1, e2)))
      ]

and gen_case depth =
  let* e = gen_expr (depth / 2) in
  let* p = gen_pattern (depth / 2) in
  return { left = p; right = e }

and gen_let_binding depth =
  let* e = gen_expr (depth / 2) in
  oneof
    [ (let* p = gen_pattern (depth / 2) in
       return (Pat_binding (p, e)))
    ; (let* id = gen_ident in
       let* pl = gen_list_helper gen_pattern (depth / 2) in
       return (Val_binding (id, pl, e)))
    ]
;;

let gen_structure depth =
  let gen_structure_item depth =
    oneof
      [ (let* e = gen_expr depth in
         return (Str_eval e))
      ; (let* r = gen_rec in
         let* lb_list = gen_list_helper gen_let_binding depth in
         return (Str_value (r, lb_list)))
      ]
  in
  gen_list_helper gen_structure_item depth
;;
