  $ ./ast_simplifier_tests.exe << EOF
  > let c = let (a, (b, c)) = (fun x -> (4, (x, x))) 4 in (a, b, c);;
  let c = let #gen_pat_expr#0 = ((fun x -> (4, (x, x))) 4) in let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let b = ((#gen_tuple_getter# 0) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in let c = ((#gen_tuple_getter# 1) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in (a, b, c);;
  
  Old types:
  val c : int * int * int
  
  New types:
  val c : 'a * 'b * 'c

  $ ./ast_simplifier_tests.exe << EOF
  > let a b = match b with | (5, (b, c), 4) -> (b, c);;
  let a b = let #gen_pat_expr#0 = b in if ((5  =  ((#gen_tuple_getter# 0) #gen_pat_expr#0))  &&  (4  =  ((#gen_tuple_getter# 2) #gen_pat_expr#0))) then let b = ((#gen_tuple_getter# 0) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in let c = ((#gen_tuple_getter# 1) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in (b, c) else (#gen_matching_failed# ());;
  
  Old types:
  val a : int * ('a * 'b) * int -> 'a * 'b
  
  New types:
  val a : 'a -> 'b * 'c

  $ ./ast_simplifier_tests.exe << EOF
  > match 1 :: [] with | 1 :: 2 :: [] -> 1 | 1 :: [] -> 2 | _ -> 3;;
  let #gen_pat_expr#0 = 1 :: [] in if (((((#gen_list_getter_length# #gen_pat_expr#0)  =  2)  &&  (1  =  (#gen_list_getter_head# #gen_pat_expr#0)))  &&  (2  =  (#gen_list_getter_head# (#gen_list_getter_tail# #gen_pat_expr#0))))  &&  ([]  =  (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0)))) then 1 else if ((((#gen_list_getter_length# #gen_pat_expr#0)  =  1)  &&  (1  =  (#gen_list_getter_head# #gen_pat_expr#0)))  &&  ([]  =  (#gen_list_getter_tail# #gen_pat_expr#0))) then 2 else if true then 3 else (#gen_matching_failed# ())
  Old types:
  
  New types:

  $ ./ast_simplifier_tests.exe < manytests/typed/015tuples.ml
  let rec fix f x = ((f (fix f)) x);;
  let map f p = let #gen_pat_expr#0 = p in let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let b = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in ((f a), (f b));;
  let fixpoly l = ((fix (fun self l -> ((map (fun li x -> ((li (self l)) x))) l))) l);;
  let feven p n = let #gen_pat_expr#0 = p in let e = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let o = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in if (n  ==  0) then 1 else (o (n  -  1));;
  let fodd p n = let #gen_pat_expr#0 = p in let e = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let o = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in if (n  ==  0) then 0 else (e (n  -  1));;
  let tie = (fixpoly (feven, fodd));;
  let rec meven n = if (n  =  0) then 1 else (modd (n  -  1)) and modd n = if (n  =  0) then 1 else (meven (n  -  1));;
  let main = let #gen_pat_expr#0 = (print_int (modd 1)) in let #gen_pat_expr#0 = (print_int (meven 2)) in let #gen_pat_expr#0 = tie in let even = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let odd = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in let #gen_pat_expr#0 = (print_int (odd 3)) in let #gen_pat_expr#0 = (print_int (even 4)) in 0;;
  
  Old types:
  val feven : 'a * (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val fodd : (int -> int) * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int) * (int -> int)
  
  New types:
  val feven : 'a -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
  val fodd : 'a -> int -> int
  val main : int
  val map : ('c -> 'b) -> 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : ('a -> 'b) * ('a -> 'b)

  $ ./ast_simplifier_tests.exe < manytests/typed/016lists.ml
  let rec length xs = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then 0 else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let h = (#gen_list_getter_head# #gen_pat_expr#0) in let tl = (#gen_list_getter_tail# #gen_pat_expr#0) in (1  +  (length tl)) else (#gen_matching_failed# ());;
  let length_tail = let rec helper acc xs = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then acc else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let h = (#gen_list_getter_head# #gen_pat_expr#0) in let tl = (#gen_list_getter_tail# #gen_pat_expr#0) in ((helper (acc  +  1)) tl) else (#gen_matching_failed# ()) in (helper 0);;
  let rec map f xs = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then [] else if (((#gen_list_getter_length# #gen_pat_expr#0)  =  1)  &&  ([]  =  (#gen_list_getter_tail# #gen_pat_expr#0))) then let a = (#gen_list_getter_head# #gen_pat_expr#0) in (f a) :: [] else if (((#gen_list_getter_length# #gen_pat_expr#0)  =  2)  &&  ([]  =  (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0)))) then let a = (#gen_list_getter_head# #gen_pat_expr#0) in let b = (#gen_list_getter_head# (#gen_list_getter_tail# #gen_pat_expr#0)) in (f a) :: (f b) :: [] else if (((#gen_list_getter_length# #gen_pat_expr#0)  =  3)  &&  ([]  =  (#gen_list_getter_tail# (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0))))) then let a = (#gen_list_getter_head# #gen_pat_expr#0) in let b = (#gen_list_getter_head# (#gen_list_getter_tail# #gen_pat_expr#0)) in let c = (#gen_list_getter_head# (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0))) in (f a) :: (f b) :: (f c) :: [] else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  4) then let a = (#gen_list_getter_head# #gen_pat_expr#0) in let b = (#gen_list_getter_head# (#gen_list_getter_tail# #gen_pat_expr#0)) in let c = (#gen_list_getter_head# (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0))) in let d = (#gen_list_getter_head# (#gen_list_getter_tail# (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0)))) in let tl = (#gen_list_getter_tail# (#gen_list_getter_tail# (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0)))) in (f a) :: (f b) :: (f c) :: (f d) :: ((map f) tl) else (#gen_matching_failed# ());;
  let rec append xs ys = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then ys else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let x = (#gen_list_getter_head# #gen_pat_expr#0) in let xs = (#gen_list_getter_tail# #gen_pat_expr#0) in x :: ((append xs) ys) else (#gen_matching_failed# ());;
  let concat = let rec helper xs = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then [] else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let h = (#gen_list_getter_head# #gen_pat_expr#0) in let tl = (#gen_list_getter_tail# #gen_pat_expr#0) in ((append h) (helper tl)) else (#gen_matching_failed# ()) in helper;;
  let rec iter f xs = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then () else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let h = (#gen_list_getter_head# #gen_pat_expr#0) in let tl = (#gen_list_getter_tail# #gen_pat_expr#0) in let #gen_pat_expr#0 = (f h) in ((iter f) tl) else (#gen_matching_failed# ());;
  let rec cartesian xs ys = let #gen_pat_expr#0 = xs in if ([]  =  #gen_pat_expr#0) then [] else if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let h = (#gen_list_getter_head# #gen_pat_expr#0) in let tl = (#gen_list_getter_tail# #gen_pat_expr#0) in ((append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys)) else (#gen_matching_failed# ());;
  let main = let #gen_pat_expr#0 = ((iter print_int) 1 :: 2 :: 3 :: []) in let #gen_pat_expr#0 = (print_int (length ((cartesian 1 :: 2 :: []) 1 :: 2 :: 3 :: 4 :: []))) in 0;;
  
  Old types:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  New types:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> 'b) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
