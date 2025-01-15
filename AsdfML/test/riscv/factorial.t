  $ dune exec riscv -- -anf -o /tmp/factorial.s <<- EOF
  > let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1)
  > let main = print_int (fact 5)
  > EOF
  ANF:
  let fact x =
         let a1 = ( < ) x 2 in
         if a1 
         then 1 
         else let a4 = ( - ) x 1 in
           let a3 = fact a4 in
           ( * ) x a3
  let main = let a6 = fact 5 in
    print_int a6
  
  $ cat /tmp/factorial.s
  
      .globl fact
      .type fact, @function
  fact:
      # args: x
      addi sp,sp,-56
      sd ra,56(sp)
      sd s0,48(sp)
      addi s0,sp,40  # Prologue ends
      sd a0,0(s0)  # x
      # Creating closure for ml_lt
      la a0,ml_lt
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      li a2,2
      call apply_closure_2
      sd a0,-8(s0)  # a1
      ld t0,-8(s0)  # a1
      beq t0,zero,.else_0
      li a0,1
      j .end_0
  .else_0:
      ld t0,0(s0)  # x
      li t1,1
      sub a0,t0,t1
      sd a0,-16(s0)  # a4
      # Creating closure for fact
      la a0,fact
      li a1,1
      call create_closure
      ld a1,-16(s0)  # a4
      call apply_closure_1
      sd a0,-24(s0)  # a3
      ld t0,0(s0)  # x
      ld t1,-24(s0)  # a3
      mul a0,t0,t1
  .end_0:
      ld s0,48(sp)  # Epilogue starts
      ld ra,56(sp)
      addi sp,sp,56
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      call runtime_init
      # Creating closure for fact
      la a0,fact
      li a1,1
      call create_closure
      li a1,5
      call apply_closure_1
      sd a0,0(s0)  # a6
      # Creating closure for ml_print_int
      la a0,ml_print_int
      li a1,1
      call create_closure
      ld a1,0(s0)  # a6
      call apply_closure_1
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
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
  >   let id = fun x -> x in
  >   helper n id
  > let main = print_int (fact 5)
  > EOF
  ANF:
  let `ll_2 cont n res = let a1 = ( * ) n res in
         cont a1
  let `helper_1 n cont =
    let a3 = ( <= ) n 1 in
    if a3 
    then cont 1 
    else let a6 = ( - ) n 1 in
      let a7 = `ll_2 cont n in
      `helper_1 a6 a7
  let `id_3 x = x
  let fact n = let a8 = `helper_1 in
    let a9 = `id_3 in
    a8 n a9
  let main = let a12 = fact 5 in
    print_int a12
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/factorial.s -o /tmp/factorial -L../../runtime/ -l:libruntime.a
  $ /tmp/factorial
  120
