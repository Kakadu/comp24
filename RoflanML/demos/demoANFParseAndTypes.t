  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let q = 1 + 2 + 3 * 4 + 5
  > EOF
  TypeEnv before ANF:
  q: int
  TypeEnv after ANF:
  q: int
  
  All types are equal!

  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let my_true x = true
  > let q = let f x y z = x + y + z in (f 1 2 3, my_true true, my_true (fun x -> x))
  > EOF
  TypeEnv before ANF:
  my_true: 'a -> bool
  q: int * bool * bool
  TypeEnv after ANF:
  my_true: 'a -> bool
  q: int * bool * bool
  
  All types are equal!

  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let q = let f x y z = x + y + z in let g x = f 1 2 in g 3
  > EOF
  TypeEnv before ANF:
  q: int -> int
  TypeEnv after ANF:
  q: int -> int
  
  All types are equal!

  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let q = if true && false then let f x = x + 1 in f 1 else let g x = x - 1 in g 1
  > EOF
  TypeEnv before ANF:
  q: int
  TypeEnv after ANF:
  q: int
  
  All types are equal!

  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let q = let x = 1 in let y = 2 in x / y
  > EOF
  TypeEnv before ANF:
  q: int
  TypeEnv after ANF:
  q: int
  
  All types are equal!

  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let q = [(fun x -> x * x) 1; (fun x -> x / x) 2]
  > EOF
  TypeEnv before ANF:
  q: int list
  TypeEnv after ANF:
  q: int list
  
  All types are equal!


  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let rec fact x k = 
  > if x = 2 then k 1 else fact (x - 1) (fun n -> k ( x * n)) 
  > EOF
  TypeEnv before ANF:
  fact: int -> (int -> 'a) -> 'a
  TypeEnv after ANF:
  fact: int -> (int -> 'a) -> 'a
  
  All types are equal!

  $ dune exec ./demoANFParseAndTypes.exe << EOF
  > let rec even x = 
  > if x = 0 then true else odd (x - 1) 
  > and 
  > odd x = if x = 0 then false else even (x - 1)
  > EOF
  TypeEnv before ANF:
  even: int -> bool
  odd: int -> bool
  TypeEnv after ANF:
  even: int -> bool
  odd: int -> bool
  
  All types are equal!
