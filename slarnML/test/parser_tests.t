  $ dune exec parser_test << EOF
  > let a = 3
  > EOF
  (let a=3)
  $ dune exec parser_test << EOF
  > let () = 0
  > EOF
  (let ()=0)
  $ dune exec parser_test << EOF
  > (fun a -> b)
  > EOF
  (fun a->b)
  $ dune exec parser_test << EOF
  > let rec a = b in (c)
  > EOF
  (let rec a=b in c)
  $ dune exec parser_test << EOF
  > if a then b else c
  > EOF
  if (a) then (b) else (c)
  $ dune exec parser_test << EOF
  > let a = 
  >   let b = 1 in
  >   let c = b in
  >   c
  > EOF
  (let a=(let b=1 in (let c=b in c)))
  $ dune exec parser_test << EOF
  > true && (a + (f false (g 3 y)) = 3  || 2)
  > EOF
  (true&&(((a+(f->false->(g->3->y)))=3)||2))
  $ dune exec parser_test << EOF
  > (a b 2 1+3 * b d (-2) (r f)) + 3
  > EOF
  ((a->b->2->(1+(3*b))->d->(-2)->(r->f))+3)
