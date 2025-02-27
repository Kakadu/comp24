  $ dune exec riscv -- -anf -o /tmp/dbg.s <<- EOF
  > let pow x n =
  >   let rec helper acc n =
  >     match n with
  >     | 0 -> acc
  >     | n -> helper (acc * x) (n - 1)
  >   in
  >   helper 1 n
  > 
  > let main = 
  >   let res = pow 2 10 in
  >   let _ = println_int res in
  >   ()
  > EOF
  ANF:
  let ll_helper_1 x acc n_0 =
         let anf1 = ( = ) n_0 0 in
         if anf1 
         then acc 
         else
           let anf3 = ( * ) acc x in
           let anf4 = ( - ) n_0 1 in
           ll_helper_1 x anf3 anf4
  let pow x n = ll_helper_1 x 1 n
  let main = let res = pow 2 10 in
    let anf6 = println_int res in
    ()
  
$ cat /tmp/dbg.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/dbg.s -o /tmp/dbg -L../../runtime/ -l:libruntime.a
  $ /tmp/dbg
  1024
