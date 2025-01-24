  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let a = [1;2;3] in
  >   print_list a
  > EOF
  ANF:
  let main = let a0 = [1; 2; 3] in
         print_list a0
  
  $ cat /tmp/lists.s
  .section .data
  
  .section .text
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      call runtime_init
      call ml_create_list
      sd a0,0(s0)  # list
      li a0,3
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a0,2
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a0,1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      sd a0,-8(s0)  # a0
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a0
      call apply_closure_1
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [1, 2, 3]


  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let a :: b :: c = [1;2;3;4] in
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = print_list c in
  >   0
  > EOF
  ANF:
  let main =
         let a1 = `list_hd [1; 2; 3; 4] in
         let a10 = `list_tl [1; 2; 3; 4] in
         let a3 = `list_hd a10 in
         let a9 = `list_tl [1; 2; 3; 4] in
         let a5 = `list_tl a9 in
         let a6 = println_int a1 in
         let a7 = println_int a3 in
         let a8 = print_list a5 in
         0
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  1
  2
  [3, 4]

  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let rec map f list = match list with
  >   | hd::tl -> (f hd) :: (map f tl) 
  >   | [] -> []
  > 
  > let sq x = x * x 
  > 
  > let main = 
  >   let full = [2;3;4] in
  >   let empty = [] in
  >   let _ = print_list (map sq full) in
  >   let _ = print_list (map sq empty) in
  >   0
  > EOF
  ANF:
  let map f list =
         let a11 = `list_is_empty list in
         let a9 = not a11 in
         let a10 = ( && ) true true in
         let a1 = ( && ) a9 a10 in
         if a1 
         then
           let a3 = `list_hd list in
           let a5 = `list_tl list in
           let a7 = f a3 in
           let a8 = map f a5 in
           ( :: ) a7 a8 
         else []
  let sq x = ( * ) x x
  let main =
    let a13 = [2; 3; 4] in
    let a14 = [] in
    let a18 = map sq a13 in
    let a15 = print_list a18 in
    let a17 = map sq a14 in
    let a16 = print_list a17 in
    0
  
  $ cat /tmp/lists.s
  .section .data
  
  .section .text
  
      .globl map
      .type map, @function
  map:
      # args: f, list
      addi sp,sp,-96
      sd ra,96(sp)
      sd s0,88(sp)
      addi s0,sp,80  # Prologue ends
      sd a0,0(s0)  # f
      sd a1,-8(s0)  # list
      # Creating closure for ml_list_is_empty
      la a0,ml_list_is_empty
      li a1,1
      call create_closure
      ld a1,-8(s0)  # list
      call apply_closure_1
      sd a0,-16(s0)  # a11
      ld t0,-16(s0)  # a11
      xori a0,t0,-1  # not a11
      sd a0,-24(s0)  # a9
      li t0,1
      li t1,1
      and a0,t0,t1  # true ( && ) true
      sd a0,-32(s0)  # a10
      ld t0,-24(s0)  # a9
      ld t1,-32(s0)  # a10
      and a0,t0,t1  # a9 ( && ) a10
      sd a0,-40(s0)  # a1
      ld t0,-40(s0)  # a1
      beq t0,zero,.else_0
      # Creating closure for ml_list_hd
      la a0,ml_list_hd
      li a1,1
      call create_closure
      ld a1,-8(s0)  # list
      call apply_closure_1
      sd a0,-48(s0)  # a3
      # Creating closure for ml_list_tl
      la a0,ml_list_tl
      li a1,1
      call create_closure
      ld a1,-8(s0)  # list
      call apply_closure_1
      sd a0,-56(s0)  # a5
      ld a0,0(s0)  # f
      ld a1,-48(s0)  # a3
      call apply_closure_1
      sd a0,-64(s0)  # a7
      # Creating closure for map
      la a0,map
      li a1,2
      call create_closure
      ld a1,0(s0)  # f
      ld a2,-56(s0)  # a5
      call apply_closure_2
      sd a0,-72(s0)  # a8
      # Creating closure for ml_list_cons
      la a0,ml_list_cons
      li a1,2
      call create_closure
      ld a1,-64(s0)  # a7
      ld a2,-72(s0)  # a8
      call apply_closure_2
      j .end_0
  .else_0:
      call ml_create_list
  .end_0:
      ld s0,88(sp)  # Epilogue starts
      ld ra,96(sp)
      addi sp,sp,96
      ret
  
      .globl sq
      .type sq, @function
  sq:
      # args: x
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      sd a0,0(s0)  # x
      ld t0,0(s0)  # x
      ld t1,0(s0)  # x
      mul a0,t0,t1  # x ( * ) x
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-88
      sd ra,88(sp)
      sd s0,80(sp)
      addi s0,sp,72  # Prologue ends
      call runtime_init
      call ml_create_list
      sd a0,0(s0)  # list
      li a0,4
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a0,3
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a0,2
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      sd a0,-8(s0)  # a13
      call ml_create_list
      sd a0,-16(s0)  # a14
      # Creating closure for sq
      la a0,sq
      li a1,1
      call create_closure
      sd a0,-24(s0)
      # Creating closure for map
      la a0,map
      li a1,2
      call create_closure
      ld a1,-24(s0)
      ld a2,-8(s0)  # a13
      call apply_closure_2
      sd a0,-32(s0)  # a18
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-32(s0)  # a18
      call apply_closure_1
      sd a0,-40(s0)  # a15
      # Creating closure for sq
      la a0,sq
      li a1,1
      call create_closure
      sd a0,-48(s0)
      # Creating closure for map
      la a0,map
      li a1,2
      call create_closure
      ld a1,-48(s0)
      ld a2,-16(s0)  # a14
      call apply_closure_2
      sd a0,-56(s0)  # a17
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-56(s0)  # a17
      call apply_closure_1
      sd a0,-64(s0)  # a16
      li a0,0
      ld s0,80(sp)  # Epilogue starts
      ld ra,88(sp)
      addi sp,sp,88
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [4, 9, 16]
  []
