  $ dune exec riscv -- -anf -o /tmp/factorial.s <<- EOF
  > let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1)
  > let main = print_int (fact 5)
  > EOF
  ANF:
  let fact x =
         let a7 = ( < ) x in
         let a1 = a7 2 in
         if a1 
         then 1 
         else
           let a3 = ( * ) x in
           let a6 = ( - ) x in
           let a5 = a6 1 in
           let a4 = fact a5 in
           a3 a4
  let main = let a9 = fact 5 in
    print_int a9
  
  $ cat /tmp/factorial.s
  
      .globl fact
      .type fact, @function
  fact:
      # args: x
      addi sp,sp,-80
      sd ra,80(sp)
      sd s0,72(sp)
      addi s0,sp,64  # Prologue ends
      sd a0,0(s0)  # x
      # Creating closure for ml_lt
      la a0,ml_lt
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      call apply_closure
      sd a0,-8(s0)  # a7
      li a1,2
      call apply_closure
      sd a0,-16(s0)  # a1
      ld t0,-16(s0)  # a1
      beq t0,zero,.else_0
      li a0,1
      j .end_0
  .else_0:
      # Creating closure for ml_mul
      la a0,ml_mul
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      call apply_closure
      sd a0,-24(s0)  # a3
      # Creating closure for ml_sub
      la a0,ml_sub
      li a1,2
      call create_closure
      ld a1,0(s0)  # x
      call apply_closure
      sd a0,-32(s0)  # a6
      li a1,1
      call apply_closure
      sd a0,-40(s0)  # a5
      # Creating closure for fact
      la a0,fact
      li a1,1
      call create_closure
      ld a1,-40(s0)  # a5
      call apply_closure
      sd a0,-48(s0)  # a4
      ld a0,-24(s0)  # a3
      ld a1,-48(s0)  # a4
      call apply_closure
  .end_0:
      ld s0,72(sp)  # Epilogue starts
      ld ra,80(sp)
      addi sp,sp,80
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
      call apply_closure
      sd a0,0(s0)  # a9
      # Creating closure for ml_print_int
      la a0,ml_print_int
      li a1,1
      call create_closure
      ld a1,0(s0)  # a9
      call apply_closure
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
  let `ll_2 cont n res = let a2 = ( * ) n in
         let a1 = a2 res in
         cont a1
  let `helper_1 n cont =
    let a12 = ( <= ) n in
    let a4 = a12 1 in
    if a4 
    then cont 1 
    else
      let a11 = ( - ) n in
      let a10 = a11 1 in
      let a7 = `helper_1 a10 in
      let a9 = `ll_2 cont in
      let a8 = a9 n in
      a7 a8
  let `id_3 x = x
  let fact n =
    let a13 = `helper_1 in
    let a14 = `id_3 in
    let a16 = a13 n in
    a16 a14
  let main = let a18 = fact 5 in
    print_int a18
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/factorial.s -o /tmp/factorial -L../../runtime/ -l:libruntime.a
  $ /tmp/factorial
  120
