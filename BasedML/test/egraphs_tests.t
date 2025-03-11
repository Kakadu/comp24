  $ dune exec ./egraphs_demo.exe << EOF
  > let a = 5 + 0
  > EOF
  let  a = 5

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = 5 - 0
  > EOF
  let  a = 5

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = 5 * 1
  > EOF
  let  a = 5

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = match a with | 5 -> 5 + 0 | 6 -> 6
  > EOF
  let  a = (match a with
  | 5 -> 5
  | 6 -> 6)

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = 1 * 1 * (0 + 5) * 1 - 0 / 1 + 0
  > EOF
  let  a = 5

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = (1241022 * 9998876) - (1241022 * 9998876)
  > EOF
  let  a = 0

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = (5 + 5) / 5
  > EOF
  let  a = ((( + ) 1) 1)

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = 5 * 2
  > EOF
  let  a = ((( + ) 5) 5)
 
  $ dune exec ./egraphs_demo.exe << EOF
  > let a = (5 / 5) * 5
  > EOF
  let  a = 5

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = (5 * 5) / 5
  > EOF
  let  a = 5

  $ dune exec ./egraphs_demo.exe << EOF
  > let overly_complicated_formula a1 a2 a3 = ((a2 * a1) / a1) - a2 + a3
  > EOF
  let  overly_complicated_formula = (fun a1 -> (fun a2 -> (fun a3 -> a3)))

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = ((var * 1) + 0) - 0 + 5
  > EOF
  let  a = ((( + ) var) 5)

  $ dune exec ./egraphs_demo.exe << EOF
  > let test1 = a * 2
  > let test1 = 2 * a
  > EOF
  let  test1 = ((( + ) a) a)
  let  test1 = ((( + ) a) a)

  $ dune exec ./egraphs_demo.exe << EOF
  > let a = (x + y) - y
  > EOF
  let  a = x

  $ dune exec ./egraphs_demo.exe << EOF
  > let test = a + 0
  > EOF
  let  test = a

  $ dune exec ./egraphs_demo.exe << EOF
  > let test = a - 0
  > EOF
  let  test = a

  $ dune exec ./egraphs_demo.exe << EOF
  > let test = a * 1
  > EOF
  let  test = a

  $ dune exec ./egraphs_demo.exe << EOF
  > let test = a / 1
  > EOF
  let  test = a

  $ dune exec ./egraphs_demo.exe << EOF
  > let test = 1 * 1 * (0 + a) * 1 - 0 / 1 + 0
  > EOF
  let  test = a
