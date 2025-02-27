  $ dune exec riscv -- -anf -o /tmp/factorial.s <<- EOF
  > let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1)
  > let main = println_int (fact 5)
  > EOF
  ANF:
  let fact x =
         let anf1 = ( < ) x 2 in
         if anf1 
         then 1 
         else let anf4 = ( - ) x 1 in
           let anf3 = fact anf4 in
           ( * ) x anf3
  let main = let anf6 = fact 5 in
    println_int anf6
  
  $ cat /tmp/factorial.s
  .section .data
  
  .section .text
  
      .globl fact
      .type fact, @function
  fact:
      # args: x
      addi sp,sp,-48
      sd ra,48(sp)
      sd s0,40(sp)
      addi s0,sp,32  # Prologue ends
      sd a0,0(s0)  # x
      # Creating closure for ml_lt
      la a0,ml_lt
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      li a2,2
      call apply_closure_2
      sd a0,-8(s0)  # anf1
      ld t0,-8(s0)  # anf1
      beq t0,zero,.else_0
      li a0,1
      j .end_0
  .else_0:
      ld t0,0(s0)  # x
      li t1,1
      sub a0,t0,t1  # x ( - ) 1
      sd a0,-16(s0)  # anf4
      # Creating closure for fact
      la a0,fact
      li a1,1
      call create_closure
      ld a1,-16(s0)  # anf4
      call apply_closure_1
      sd a0,-24(s0)  # anf3
      ld t0,0(s0)  # x
      ld t1,-24(s0)  # anf3
      mul a0,t0,t1  # x ( * ) anf3
  .end_0:
      ld s0,40(sp)  # Epilogue starts
      ld ra,48(sp)
      addi sp,sp,48
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      call runtime_init
      # Creating closure for fact
      la a0,fact
      li a1,1
      call create_closure
      li a1,5
      call apply_closure_1
      sd a0,0(s0)  # anf6
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,0(s0)  # anf6
      call apply_closure_1
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret

  $ riscv64-unknown-linux-gnu-gcc /tmp/factorial.s -o /tmp/factorial -L../../runtime/ -l:libruntime.a
  $ /tmp/factorial
  120

  $ dune exec riscv -- -anf -o /tmp/factorial.s <<- EOF
  > let fact n =
  >   let rec helper n cont =
  >     if n <= 1 then 
  >       cont 1
  >     else 
  >       helper (n - 1) (fun res -> cont (n * res)) 
  >   in 
  >   helper n (fun x -> x)
  > let main = println_int (fact 5)
  > EOF
  ANF:
  let ll_2 cont n_0 res = let anf1 = ( * ) n_0 res in
         cont anf1
  let ll_helper_1 n_0 cont =
    let anf3 = ( <= ) n_0 1 in
    if anf3 
    then cont 1 
    else
      let anf6 = ( - ) n_0 1 in
      let anf7 = ll_2 cont n_0 in
      ll_helper_1 anf6 anf7
  let ll_3 x = x
  let fact n = ll_helper_1 n ll_3
  let main = let anf10 = fact 5 in
    println_int anf10
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/factorial.s -o /tmp/factorial -L../../runtime/ -l:libruntime.a
  $ /tmp/factorial
  120


  $ dune exec riscv -- -anf -o /tmp/factorial.s <<- EOF
  > let fact n =
  >   let rec helper n cont =
  >     if n <= 1 then 
  >       cont 1
  >     else 
  >       helper (n - 1) (fun res -> cont (n * res)) 
  >   in 
  >   helper n (fun x -> x)
  > let main = println_int (fact 5)
  > EOF
  ANF:
  let ll_2 cont n_0 res = let anf1 = ( * ) n_0 res in
         cont anf1
  let ll_helper_1 n_0 cont =
    let anf3 = ( <= ) n_0 1 in
    if anf3 
    then cont 1 
    else
      let anf6 = ( - ) n_0 1 in
      let anf7 = ll_2 cont n_0 in
      ll_helper_1 anf6 anf7
  let ll_3 x = x
  let fact n = ll_helper_1 n ll_3
  let main = let anf10 = fact 5 in
    println_int anf10
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/factorial.s -o /tmp/factorial -L../../runtime/ -l:libruntime.a
  $ /tmp/factorial
  120


  $ dune exec riscv -- -o /tmp/factorial.s <<- EOF
  > let fib n =
  >   let rec helper acc n =
  >     match (n, acc) with
  >     | (0, x :: _) -> x
  >     | (_, x :: y :: _) -> helper ((x + y) :: acc) (n - 1)
  >     | _ -> -1
  >   in
  >   helper [ 1; 1 ] (n - 2)
  > let main = 
  >   let _ = println_int (fib 3) in
  >   let _ = println_int (fib 4) in
  >   let _ = println_int (fib 5) in
  >   let _ = println_int (fib 6) in
  >   let _ = println_int (fib 7) in
  >   let _ = println_int (fib 8) in
  >   let _ = println_int (fib 9) in
  >   let _ = println_int (fib 10) in
  >   ()
  > EOF
  $ riscv64-unknown-linux-gnu-gcc /tmp/factorial.s -o /tmp/factorial -L../../runtime/ -l:libruntime.a
  $ /tmp/factorial
  2
  3
  5
  8
  13
  21
  34
  55

