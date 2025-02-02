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
  > let a = 1 * 1 * (5 + 0) * 1 - 0 / 1 + 0
  > EOF
  let  a = 5



