  $ dune exec ./match_elimination_demo.exe << EOF
  > let prog = let (difficult_pattern, nasty_pattern) = (5, 6) in a
  > EOF
  let  prog  = (match (5, 6) with 
  | (difficult_pattern, nasty_pattern) -> a)

  $ dune exec ./match_elimination_demo.exe << EOF
  > let prog = let (difficult_pattern, nasty_pattern) = let (difficult_pattern, nasty_pattern) = (5, 6) in a in a
  > EOF
  let  prog  = (match (match (5, 6) with 
  | (difficult_pattern, nasty_pattern) -> a) with 
  | (difficult_pattern, nasty_pattern) -> a)

