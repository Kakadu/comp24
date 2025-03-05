;  $ dune exec riscv -- -anf -o /tmp/dbg.s <<- EOF
;  > let pow x n =
;  >   let rec helper acc n =
;  >     match n with
;  >     | 0 -> acc
;  >     | n -> helper (acc * x) (n - 1)
;  >   in
;  >   helper 1 n
;  > 
;  > let main = 
;  >   let res = pow 2 10 in
;  >   let _ = println_int res in
;  >   ()
;  > EOF
;  ANF:
;  let ll_helper_1 x acc n_0 =
;         let anf1 = ( = ) n_0 0 in
;         if anf1 
;         then acc 
;         else
;           let anf3 = ( * ) acc x in
;           let anf4 = ( - ) n_0 1 in
;           ll_helper_1 x anf3 anf4
;  let pow x n = ll_helper_1 x 1 n
;  let main = let res = pow 2 10 in
;    let anf6 = println_int res in
;    ()
;  
;$ cat /tmp/dbg.s
;  $ riscv64-unknown-linux-gnu-gcc /tmp/dbg.s -o /tmp/dbg -L../../runtime/ -l:libruntime.a
;  $ RUST_LOG=debug /tmp/dbg
;  1024


  $ dune exec riscv -- -anf -o /tmp/dbg.s <<- EOF
  > let wrap f = if 1 = 1 then f else f
  > let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  > let main =
  >   let rez =
  >     ((wrap test10) 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
  >   in
  >   let () = println_int rez in
  >   0
  > EOF
  ANF:
  let wrap f = let anf1 = ( = ) 1 1 in
         if anf1 
         then f 
         else f
  let test10 a b c d e f_0 g h i j =
    let anf10 = ( + ) a b in
    let anf9 = ( + ) anf10 c in
    let anf8 = ( + ) anf9 d in
    let anf7 = ( + ) anf8 e in
    let anf6 = ( + ) anf7 f_0 in
    let anf5 = ( + ) anf6 g in
    let anf4 = ( + ) anf5 h in
    let anf3 = ( + ) anf4 i in
    ( + ) anf3 j
  let main =
    let rez =
      wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let anf14 = println_int rez in
    let anf12 = ( = ) anf14 () in
    if anf12 
    then 0 
    else panic ()
  
$ cat /tmp/dbg.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/dbg.s -o /tmp/dbg -L../../runtime/ -l:libruntime.a
  $ RUST_LOG=debug /tmp/dbg
  [runtime::tuple]: Created tuple of size 5 at 0x240330
  [runtime::tuple]: Set [0] = 100000 in [100000, 0, 0, 0, 0] at 0x240330
  [runtime::tuple]: Set [1] = 1000000 in [100000, 1000000, 0, 0, 0] at 0x240330
  [runtime::tuple]: Set [2] = 10000000 in [100000, 1000000, 10000000, 0, 0] at 0x240330
  [runtime::tuple]: Set [3] = 100000000 in [100000, 1000000, 10000000, 100000000, 0] at 0x240330
  [runtime::tuple]: Set [4] = 1000000000 in [100000, 1000000, 10000000, 100000000, 1000000000] at 0x240330
  [runtime::closure]: Creating Closure { fn_ptr: 0x1e77a, arity: 10, args: dec[] / hex[] } at 0x2405e0
  [runtime::closure]: Creating Closure { fn_ptr: 0x1e740, arity: 1, args: dec[] / hex[] } at 0x394d20
  [runtime::closure]: Tuple with arguments [100000, 1000000, 10000000, 100000000, 1000000000] at 0x240330
  [runtime::closure]: Applying Closure { fn_ptr: 0x1e740, arity: 1, args: dec[2360800, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000] / hex[2405e0, 1, a, 64, 3e8, 2710, 186a0, f4240, 989680, 5f5e100, 3b9aca00] }
  [runtime::closure]: Too many args, [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000] goes to next closure
  [runtime::closure]: Applying Closure { fn_ptr: 0x1e740, arity: 1, args: dec[2360800] / hex[2405e0] }
  [runtime::closure]: Creating Closure { fn_ptr: 0x24608 (ml_le), arity: 2, args: dec[] / hex[] } at 0x2601f0
  [runtime::closure]: Applying Closure { fn_ptr: 0x24608 (ml_le), arity: 2, args: dec[1, 1] / hex[1, 1] }
  [runtime::closure]: Closure result: 1 / 0x1
  [runtime::closure]: Closure result: 2360800 / 0x2405e0
  [runtime::closure]: Applying Closure { fn_ptr: 0x1e77a, arity: 10, args: dec[1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000] / hex[1, a, 64, 3e8, 2710, 186a0, f4240, 989680, 5f5e100, 3b9aca00] }
  [runtime::tuple]: Getting 0 of [10000000, 100000000, 1000000000] at 0x241f10
  [runtime::tuple]: Getting 1 of [10000000, 100000000, 1000000000] at 0x241f10
  [runtime::tuple]: Getting 2 of [10000000, 100000000, 1000000000] at 0x241f10
  [runtime::closure]: Closure result: 1111111111 / 0x423a35c7
  [runtime::closure]: Creating Closure { fn_ptr: 0x24266 (ml_div), arity: 1, args: dec[] / hex[] } at 0x130e330
  [runtime::closure]: Applying Closure { fn_ptr: 0x24266 (ml_div), arity: 1, args: dec[1111111111] / hex[423a35c7] }
  1111111111
  [runtime::closure]: Closure result: 0 / 0x0
  [runtime::closure]: Creating Closure { fn_ptr: 0x24608 (ml_le), arity: 2, args: dec[] / hex[] } at 0x130aa60
  [runtime::closure]: Applying Closure { fn_ptr: 0x24608 (ml_le), arity: 2, args: dec[0, 0] / hex[0, 0] }
  [runtime::closure]: Closure result: 1 / 0x1
