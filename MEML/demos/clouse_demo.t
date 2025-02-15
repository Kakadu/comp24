
  $ ./clouse_demo.exe << EOF
  > let sum x = let num2 y = x + y in test2
  let  sum = (fun x -> 
    let  num2 = (fun test2 -> (fun x -> (fun y -> x + y)))
    in test2)

  $ ./clouse_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  test1 = (fun (x, y) -> 
    let  test2 = (fun x -> (fun y -> (fun i -> (x, y, i))))
    in test2 x y)

  $ ./clouse_demo.exe << EOF
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva) 0 in
  >   odin_plus_dva_plus 5
  let  vosem = 
    let  odin = 1
    in 
    let  dva = 2
    in 
    let  odin_plus_dva_plus = (fun dva -> (fun odin -> (fun x -> x + (fun dva -> (fun odin -> (fun i -> odin + dva))) dva odin 0)))
    in odin_plus_dva_plus dva odin 5

  $ ./clouse_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b = 6
  let  i = 1
  let  sem = (fun num1 -> (fun num2 -> num1 + num2)) b i

  $ ./clouse_demo.exe << EOF
  > let rec fac_cps num =
  >   let helper num acc =
  >     if num = 0
  >     then acc 1
  >     else fac_cps (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let rec fac_cps = (fun num -> 
    let  helper = (fun fac_cps -> (fun num -> (fun acc -> 
    if num = 0
    then acc 1
    else fac_cps num - 1 (fun acc -> (fun num -> (fun t -> acc num * t))) acc num)))
    in helper fac_cps num (fun x -> x))
