  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let main = 
  >   let a = [1;2;3] in
  >   print_list a
  > EOF
  ANF:
  let main = let a = [1; 2; 3] in
         print_list a
  
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
      sd a0,-8(s0)  # a
      # Creating closure for ml_print_list
      la a0,ml_print_list
      li a1,1
      call create_closure
      ld a1,-8(s0)  # a
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
  >   let list = [1;2;3;4] in
  >   let a :: b :: c = list in
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = print_list c in
  >   0
  > EOF
  ANF:
  let main =
         let list = [1; 2; 3; 4] in
         let anf15 = `list_is_empty list in
         let anf11 = not anf15 in
         let anf14 = `list_tl list in
         let anf13 = `list_is_empty anf14 in
         let anf12 = not anf13 in
         let anf1 = ( && ) anf11 anf12 in
         if anf1 
         then
           let a = `list_hd list in
           let anf9 = `list_tl list in
           let b = `list_hd anf9 in
           let anf8 = `list_tl list in
           let c = `list_tl anf8 in
           let _ = println_int a in
           let _ = println_int b in
           let _ = print_list c in
           0 
         else panic ()
  
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  1
  2
  [3, 4]

  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let length list = 
  >   let rec helper acc list = match list with
  >     | [] -> acc 
  >     | _ :: tl -> helper (acc + 1) tl
  >   in
  >   helper 0 list
  > 
  > let main = 
  >   let full = [2;3;4] in
  >   let empty = [] in
  >   let _ = println_int (length full) in
  >   let _ = println_int (length empty) in
  >   0
  > EOF
  ANF:
  let ll_helper_1 acc list =
         let anf1 = `list_is_empty list in
         if anf1 
         then acc 
         else
           let anf8 = `list_is_empty list in
           let anf3 = not anf8 in
           if anf3 
           then
             let tl = `list_tl list in
             let anf6 = ( + ) acc 1 in
             ll_helper_1 anf6 tl 
           else panic ()
  let length list = ll_helper_1 0 list
  let main =
    let full = [2; 3; 4] in
    let empty = [] in
    let anf13 = length full in
    let _ = println_int anf13 in
    let anf12 = length empty in
    let _ = println_int anf12 in
    0
  
$ cat /tmp/lists.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  3
  0

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
         let anf10 = `list_is_empty list in
         let anf1 = not anf10 in
         if anf1 
         then
           let hd = `list_hd list in
           let tl = `list_tl list in
           let anf5 = f hd in
           let anf6 = map f tl in
           ( :: ) anf5 anf6 
         else let anf8 = `list_is_empty list in
           if anf8 
           then [] 
           else panic ()
  let sq x = ( * ) x x
  let main =
    let full = [2; 3; 4] in
    let empty = [] in
    let anf15 = map sq full in
    let _ = print_list anf15 in
    let anf14 = map sq empty in
    let _ = print_list anf14 in
    0
  
$ cat /tmp/lists.s
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  [4, 9, 16]
  []

  $ dune exec riscv -- -anf -o /tmp/lists.s <<- EOF
  > let [a; b; c] = [1; 2; 3]
  > let main = 
  >   let _ = println_int a in
  >   let _ = println_int b in
  >   let _ = println_int c in
  >   0
  > EOF
  ANF:
  let __temp_match_0 =
         let __temp_match_0 = [1; 2; 3] in
         let anf3 = `list_len __temp_match_0 in
         let anf1 = ( = ) anf3 3 in
         if anf1 
         then __temp_match_0 
         else panic ()
  let __list_0 = __temp_match_0
  let a = `list_field __list_0 0
  let b = `list_field __list_0 1
  let c = `list_field __list_0 2
  let main =
    let _ = println_int a in
    let _ = println_int b in
    let _ = println_int c in
    0
  
  init___temp_match_0 ANF:
  let init___temp_match_0 _ =
    let __temp_match_0 = [1; 2; 3] in
    let anf3 = `list_len __temp_match_0 in
    let anf1 = ( = ) anf3 3 in
    if anf1 
    then __temp_match_0 
    else panic ()
  
  init___list_0 ANF:
  let init___list_0 _ = __temp_match_0
  
  init_a ANF:
  let init_a _ = `list_field __list_0 0
  
  init_b ANF:
  let init_b _ = `list_field __list_0 1
  
  init_c ANF:
  let init_c _ = `list_field __list_0 2
  
  $ cat /tmp/lists.s  
  .section .data
  __temp_match_0: .dword 0
  __list_0: .dword 0
  a: .dword 0
  b: .dword 0
  c: .dword 0
  
  .section .text
  
      .globl init___temp_match_0
      .type init___temp_match_0, @function
  init___temp_match_0:
      # args: _
      addi sp,sp,-64
      sd ra,64(sp)
      sd s0,56(sp)
      addi s0,sp,48  # Prologue ends
      sd a0,0(s0)  # _
      call ml_create_list
      sd a0,-8(s0)  # list
      li a0,3
      ld a1,-8(s0)
      call ml_list_cons
      sd a0,-8(s0)
      li a0,2
      ld a1,-8(s0)
      call ml_list_cons
      sd a0,-8(s0)
      li a0,1
      ld a1,-8(s0)
      call ml_list_cons
      sd a0,-8(s0)
      sd a0,-16(s0)  # __temp_match_0
      sd a0,-24(s0)
      # Creating closure for ml_list_len
      la a0,ml_list_len
      li a1,1
      call create_closure
      ld a1,-24(s0)
      call apply_closure_1
      sd a0,-32(s0)  # anf3
      # Creating closure for ml_eq
      la a0,ml_eq
      li a1,2
      call create_closure
      ld a1,-32(s0)  # anf3
      li a2,3
      call apply_closure_2
      sd a0,-40(s0)  # anf1
      ld t0,-40(s0)  # anf1
      beq t0,zero,.else_0
      ld a0,-16(s0)  # __temp_match_0
      j .end_0
  .else_0:
      # Creating closure for ml_panic
      la a0,ml_panic
      li a1,1
      call create_closure
      li a1,0
      call apply_closure_1
  .end_0:
      la t1,__temp_match_0
      sd a0,0(t1)
      ld s0,56(sp)  # Epilogue starts
      ld ra,64(sp)
      addi sp,sp,64
      ret
  
      .globl init___list_0
      .type init___list_0, @function
  init___list_0:
      # args: _
      addi sp,sp,-24
      sd ra,24(sp)
      sd s0,16(sp)
      addi s0,sp,8  # Prologue ends
      sd a0,0(s0)  # _
      la a0,__temp_match_0
      ld a0,0(a0)
      la t1,__list_0
      sd a0,0(t1)
      ld s0,16(sp)  # Epilogue starts
      ld ra,24(sp)
      addi sp,sp,24
      ret
  
      .globl init_a
      .type init_a, @function
  init_a:
      # args: _
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # _
      la a0,__list_0
      ld a0,0(a0)
      sd a0,-8(s0)
      # Creating closure for ml_list_field
      la a0,ml_list_field
      li a1,2
      call create_closure
      ld a1,-8(s0)
      li a2,0
      call apply_closure_2
      la t1,a
      sd a0,0(t1)
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  
      .globl init_b
      .type init_b, @function
  init_b:
      # args: _
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # _
      la a0,__list_0
      ld a0,0(a0)
      sd a0,-8(s0)
      # Creating closure for ml_list_field
      la a0,ml_list_field
      li a1,2
      call create_closure
      ld a1,-8(s0)
      li a2,1
      call apply_closure_2
      la t1,b
      sd a0,0(t1)
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  
      .globl init_c
      .type init_c, @function
  init_c:
      # args: _
      addi sp,sp,-32
      sd ra,32(sp)
      sd s0,24(sp)
      addi s0,sp,16  # Prologue ends
      sd a0,0(s0)  # _
      la a0,__list_0
      ld a0,0(a0)
      sd a0,-8(s0)
      # Creating closure for ml_list_field
      la a0,ml_list_field
      li a1,2
      call create_closure
      ld a1,-8(s0)
      li a2,2
      call apply_closure_2
      la t1,c
      sd a0,0(t1)
      ld s0,24(sp)  # Epilogue starts
      ld ra,32(sp)
      addi sp,sp,32
      ret
  
      .globl main
      .type main, @function
  main:
      addi sp,sp,-64
      sd ra,64(sp)
      sd s0,56(sp)
      addi s0,sp,48  # Prologue ends
      call runtime_init
      call init___temp_match_0
      call init___list_0
      call init_a
      call init_b
      call init_c
      la a0,a
      ld a0,0(a0)
      sd a0,0(s0)
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,0(s0)
      call apply_closure_1
      sd a0,-8(s0)  # _
      la a0,b
      ld a0,0(a0)
      sd a0,-16(s0)
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,-16(s0)
      call apply_closure_1
      sd a0,-24(s0)  # _
      la a0,c
      ld a0,0(a0)
      sd a0,-32(s0)
      # Creating closure for ml_println_int
      la a0,ml_println_int
      li a1,1
      call create_closure
      ld a1,-32(s0)
      call apply_closure_1
      sd a0,-40(s0)  # _
      li a0,0
      ld s0,56(sp)  # Epilogue starts
      ld ra,64(sp)
      addi sp,sp,64
      ret
  $ riscv64-unknown-linux-gnu-gcc /tmp/lists.s -o /tmp/lists -L../../runtime/ -l:libruntime.a
  $ /tmp/lists
  1
  2
  3

