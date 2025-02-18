
  $ ./lamdalift_demo.exe << EOF
  > let sum x = let num2 y = x + y in test2
  let  lambada0 test2 x y  = (x + y)
  let  sum x  = test2

  $ ./lamdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  lambada0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((lambada0 x) y)

  $ ./lamdalift_demo.exe << EOF
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva) 0 in
  >   odin_plus_dva_plus 5
  let  lambada1 dva odin i  = (odin + dva)
  let  lambada0 dva odin x  = (x + (((lambada1 dva) odin) 0))
  let  vosem  = 
    let  odin = 1
    in 
    let  dva = 2
    in (((lambada0 dva) odin) 5)

  $ ./lamdalift_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let lambada0 = 1;;
  > let lambada1 = 3;;
  > let lambada3 = 4;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b  = 6
  let  i  = 1
  let  lambada0  = 1
  let  lambada1  = 3
  let  lambada3  = 4
  let  lambada2 num1 num2  = (num1 + num2)
  let  sem  = ((lambada2 b) i)

  $ ./lamdalift_demo.exe << EOF
  > let rec fac_cps num =
  >   let helper num acc =
  >     if num = 0
  >     then acc 1
  >     else fac_cps (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let  lambada1 acc num t  = (acc (num * t))
  let  lambada0 fac_cps num acc  = 
    if (num = 0)
    then (acc 1)
    else ((fac_cps (num - 1)) ((lambada1 acc) num))
  let  lambada2 x  = x
  let rec fac_cps num  = (((lambada0 fac_cps) num) lambada2)
