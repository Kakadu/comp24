  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so 
  > let a = 1
  > let f x =
  >   let b = 2 in
  >   fun y -> y + a + x + b
  > let main =
  >   let () = print_int (f 3 4) in
  >   0
  10

  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so 
  > let add x y = x + y
  > let main =
  >   let add5 = add 5 in
  >   let () = print_int (add5 1) in
  >   0
  6

  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so 
  > let add x y = x + y
  > let add5 = add 5
  > let main =
  >   let () = print_int (add5 1) in
  >   0
  6


  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so
  > let sum_pair (a, b) = a + b
  > let main = 
  >   let () = print_int (sum_pair (1, 2)) in
  >   0
  3

  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so
  > let sum_list l =
  >   let rec helper acc l =
  >   match l with
  >   | x::xs -> helper (acc + x) xs
  >   | [] -> acc in
  >   helper 0 l
  > let main =
  >   let () = print_int (sum_list [1;2;3;4;5]) in
  >   0
  15

 
  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so 
  > let rec fac_cps x k =
  >   if x <= 1 then k 1 else fac_cps (x-1) (fun res -> k (res * x))
  > let main =
  >   let id x = x in
  >   let () = print_int (fac_cps 10 id) in
  >   0
  3628800
 
  $ dune exec llvm_codegen 2>&1 <<- EOF | lli-17 -load ../runtime/runtime.so
  > let rec map f list =
  > match list with
  > | [] -> []
  > | x::xs -> (f x)::(map f xs)
  > let rec iter action list =
  >  match list with
  >  | [] -> ()
  >  | x::xs -> let () = action x in let () =  iter action xs in ()
  > let main =
  >  let square x = x * x in
  >   let list = [1; 2; 3; 4; 5] in
  >   let squared = map square list in
  >   let () = iter print_int squared in
  >   0
  1
  4
  9
  16
  25

  $ dune exec llvm_codegen 2>&1 < manytests/typed/001fac.ml | lli-17 -load ../runtime/runtime.so
  24
  $ dune exec llvm_codegen 2>&1 < manytests/typed/002fac.ml | lli-17 -load ../runtime/runtime.so
  24
  $ dune exec llvm_codegen 2>&1 < manytests/typed/003fib.ml | lli-17 -load ../runtime/runtime.so
  3
  3
  $ dune exec llvm_codegen 2>&1 < manytests/typed/004manyargs.ml | lli-17 -load ../runtime/runtime.so
  1111111111
  1
  10
  100

$ dune exec llvm_codegen 2>&1 < manytests/typed/005fix.ml | lli-17 -load ../runtime/runtime.so


$ dune exec llvm_codegen 2>&1 < manytests/typed/015tuples.ml | lli-17 -load ../runtime/runtime.so
lli-17: lli: <stdin>:1:1: error: expected top-level entity



  $ dune exec llvm_codegen 2>&1 < manytests/typed/016lists.ml | lli-17 -load ../runtime/runtime.so
  lli-17: lli: <stdin>:455:47: error: '%phi_279' defined with type 'ptr' but expected 'i64'
    %phi_281 = phi i64 [ %ys_22, %then_259 ], [ %phi_279, %merge_278 ]
                                                ^
  
  [1]
