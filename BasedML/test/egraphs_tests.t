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
  > let a = 5 / 1
  > EOF
  let  a = 5

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


