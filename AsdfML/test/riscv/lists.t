  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let a = [1;2;3] in
  >   print_list a
  > EOF
  ANF:
  let main = let a0 = [1; 2; 3] in
         print_list a0
  
  $ cat /tmp/lists.s
  
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
      li a1,3
      mv a0,a1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a1,2
      mv a0,a1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a1,1
      mv a0,a1
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
  > let rec map f list = match list with
  > | [] -> []
  > | hd::tl -> (f hd) :: (map f tl)
  > 
  > let sq x = x * x 
  > 
  > let main = 
  >   let x = [2;3;4] in
  >   let res = map sq x in
  >   print_list res
  > EOF
  ANF:
  let map f list =
         let a1 = `list_is_empty list in
         if a1 
         then [] 
         else
           let a3 = `list_hd list in
           let a5 = `list_tl list in
           let a7 = f a3 in
           let a8 = map f a5 in
           ( :: ) a7 a8
  let sq x = ( * ) x x
  let main = let a10 = [2; 3; 4] in
    let a12 = map sq a10 in
    print_list a12
  
  $ cat /tmp/lists.s
  
      .globl map
      .type map, @function
  map:
      # args: f, list
      addi sp,sp,-72
      sd ra,72(sp)
      sd s0,64(sp)
      addi s0,sp,56  # Prologue ends
      sd a0,0(s0)  # f
      sd a1,-8(s0)  # list
      # Creating closure for ml_list_is_empty
      la a0,ml_list_is_empty
      li a1,1
      call create_closure
      ld a1,-8(s0)  # list
      call apply_closure_1
      sd a0,-16(s0)  # a1
      ld t0,-16(s0)  # a1
      beq t0,zero,.else_0
      call ml_create_list
      j .end_0
  .else_0:
      # Creating closure for ml_list_hd
      la a0,ml_list_hd
      li a1,1
      call create_closure
      ld a1,-8(s0)  # list
      call apply_closure_1
      sd a0,-24(s0)  # a3
      # Creating closure for ml_list_tl
      la a0,ml_list_tl
      li a1,1
      call create_closure
      ld a1,-8(s0)  # list
      call apply_closure_1
      sd a0,-32(s0)  # a5
      ld a0,0(s0)  # f
      ld a1,-24(s0)  # a3
      call apply_closure_1
      sd a0,-40(s0)  # a7
      # Creating closure for map
      la a0,map
      li a1,2
      call create_closure
      ld a1,0(s0)  # f
      ld a2,-32(s0)  # a5
      call apply_closure_2
      sd a0,-48(s0)  # a8
      # Creating closure for ml_list_cons
      la a0,ml_list_cons
      li a1,2
      call create_closure
      ld a1,-40(s0)  # a7
      ld a2,-48(s0)  # a8
      call apply_closure_2
  .end_0:
      ld s0,64(sp)  # Epilogue starts
      ld ra,72(sp)
      addi sp,sp,72
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
      addi sp,sp,-48
      sd ra,48(sp)
      sd s0,40(sp)
      addi s0,sp,32  # Prologue ends
      call runtime_init
      call ml_create_list
      sd a0,0(s0)  # list
      li a1,4
      mv a0,a1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a1,3
      mv a0,a1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      li a1,2
      mv a0,a1
      ld a1,0(s0)
      call ml_list_cons
      sd a0,0(s0)
      sd a0,-8(s0)  # a10
      # Creating closure for sq
      la a0,sq
      li a1,1
      call create_closure
      sd a0,-16(s0)
      # Creating closure for map
      la a0,map
      li a1,2
      call create_closure
      ld a1,-16(s0)
      ld a2,-8(s0)  # a10
      call apply_closure_2
      sd a0,-24(s0)  # a12
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-24(s0)  # a12
      call apply_closure_1
      ld s0,40(sp)  # Epilogue starts
      ld ra,48(sp)
      addi sp,sp,48
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [4, 9, 16]
