  $ dune exec ./match_elimination_demo.exe << EOF
  > let prog = let (difficult_pattern, nasty_pattern) = (5, 6) in a
  > EOF
  let  prog  = (let  difficult_pattern = 5 in (let  nasty_pattern = 6 in a))

  $ dune exec ./match_elimination_demo.exe << EOF
  > let prog = let (difficult_pattern, nasty_pattern) = let (difficult_pattern, nasty_pattern) = (5, 6) in a in a
  > EOF
  let  prog  = (let  difficult_pattern = ((get_field (let  difficult_pattern = 5 in (let  nasty_pattern = 6 in a))) 0) in (let  nasty_pattern = ((get_field (let  difficult_pattern = 5 in (let  nasty_pattern = 6 in a))) 1) in a))

