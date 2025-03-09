  $ dune exec ./demoANFTypes.exe << EOF
  > let q = 1 + 2 + 3 * 4 + 5
  > EOF
  TypeEnv before ANF:
  q: int
  TypeEnv after ANF:
  q: int
  
  All types are equal!

  $ dune exec ./demoANFTypes.exe << EOF
  > let q = let f x y z = x + y + z in let g x = f 1 2 in g 3
  > EOF
  TypeEnv before ANF:
  q: int -> int
  TypeEnv after ANF:
  q: int -> int
  
  All types are equal!

  $ dune exec ./demoANFTypes.exe << EOF
  > let q = if true && false then let f x = x + 1 in f 1 else let g x = x - 1 in g 1
  > EOF
  TypeEnv before ANF:
  q: int
  TypeEnv after ANF:
  q: int
  
  All types are equal!

  $ dune exec ./demoANFTypes.exe << EOF
  > let q = let x = 1 in let y = 2 in x / y
  > EOF
  TypeEnv before ANF:
  q: int
  TypeEnv after ANF:
  q: int
  
  All types are equal!

  $ dune exec ./demoANFTypes.exe << EOF
  > let q = [(fun x -> x * x) 1; (fun x -> x / x) 2]
  > EOF
  TypeEnv before ANF:
  q: int list
  TypeEnv after ANF:
  q: int list
  
  All types are equal!




