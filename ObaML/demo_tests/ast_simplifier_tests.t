  $ ./ast_simplifier_tests.exe << EOF
  > 
  > let rec map f xs =
  > match xs with
  > | a::[] -> [ a ]
  > | a::[] -> [ a ]
  let rec map f xs = let #gen_pat_expr#0 = xs in if (((#gen_list_getter_length# #gen_pat_expr#0)  =  1)  &&  ([]  =  (#gen_list_getter_tail# #gen_pat_expr#0))) then let a = (#gen_list_getter_head# #gen_pat_expr#0) in a :: [] else if (((#gen_list_getter_length# #gen_pat_expr#0)  =  1)  &&  ([]  =  (#gen_list_getter_tail# #gen_pat_expr#0))) then let a = (#gen_list_getter_head# #gen_pat_expr#0) in a :: [] else (#gen_matching_failed# ());;
  
  Old typed:
  val map : 'a -> 'b list -> 'b list
  
  New types:
  val map : 'a -> 'b list -> 'b list
