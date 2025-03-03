
  $ ./llvm_demo.exe << EOF | lli-17 -load ../lib/runtime.so 
  > let sum a b = a + b 
  > let dva = 2
  > let bul = 1 = 1
  > let main = let () = print_int (sum dva 1) in let () = print_int 2 in 0
  3
  2
  $ ./llvm_demo.exe << EOF | lli-17 -load ../lib/runtime.so 
  > let rec sum a = if a = 0 then 0 else a + sum (a - 1) 
  > let dva = 2
  > let bul = 1 = 1
  > let main = let () = print_int (sum dva) in let () = print_int 2 in 0
  3
  2
