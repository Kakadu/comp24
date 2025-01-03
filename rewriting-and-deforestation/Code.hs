module Code where

import Prelude hiding (map, (.))

(?) :: (b -> c) -> (a -> b) -> a -> c
(?) a b = undefined

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

{-# RULES
"map/map" forall f g xs.
    map f (map g xs) = map (f . g) xs #-}

foo :: p1 -> p2 -> a
foo x y = undefined

{-# RULES
"commute" forall x y. foo x y = foo y x #-}

all :: (a -> Bool) -> [a] -> Bool
all p xs = and (map p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = h xs
 where h []       = True
       h (x : xs) = p x && h xs

case (C2 t1 t2 t3) of
  C1 x1 x2    -> e1
  C2 x1 x2 x3 -> e2
  C3 x1       -> e3

let x1 = t1
    x2 = t2
    x3 = t3
in e2

foldr :: (a -> b -> b) -> b -> [a] -> [b]
foldr k z []       = z
foldr k z (x : xs) = k x (foldr k z xs)

and xs       = foldr (&&) True xs
map f xs     = foldr (\a b -> f a : b) [] xs
xs ++ ys     = foldr (:) ys xs
concat xs    = foldr (++) [] xs
foldl f z xs = foldr (\b g a -> g (f a b)) id xs z

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

{-# RULES
"foldr/build"
  forall k z (g :: forall b. (a -> b -> b) -> b -> b) .
  foldr k z (build g) = g k z #-}

[]        = build (\c n -> n)
x : xs    = build (\c n -> c x (foldr c n xs))
map f xs  = build (\c n -> foldr (\a b -> c (f a) b) n xs)
xs ++ ys  = build (\c n -> foldr c (foldr c n ys) xs)
concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
repeat x  = build (\c n -> let r = c x r in r)

(||) :: Bool -> Bool -> Bool
True  || _ =  True
False || x =  x

repeat True -- [True, True, ..]

repeat x = build (\c n -> let r = c x r in r)

x = foldr (||) False (repeat True)

x = foldr (||) False (repeat True) -- repeat <-> build
  = foldr (||) False (build (\c n -> let r = c True r in r)) -- foldr/build
  = (\c n -> let r = c True r in r) (||) False -- beta-reduce
  = let r = (||) True r in r -- beta-reduce
  = True

all :: (a -> Bool) -> [a] -> Bool
all p xs = and (map p xs)
         = foldr (&&) True (map p xs)
         = foldr (&&) True (build (\c n -> foldr (\a b -> c (p a) b) n xs))
         = (\c n -> foldr (\a b -> c (p a) b) n xs) (&&) True
         = foldr (\a b -> (&&) (p a) b) True xs
         = foldr (\a b -> p a && b) True xs -- inline foldr
         = h xs
 where h [] = True
       h (x : xs) = p x && h xs

-- (down 5) = [5, 4, 3, 2, 1]
down :: Int -> [Int]
down v = build (\c n -> down' v c n)

down' 0 cons nil = nil
down' v cons nil = cons v (down' (v - 1) cons nil)

x = sum (down 5)
  = foldr (+) 0 (down 5)
  = foldr (+) 0 (build (\c n -> down' 5 c n))
  = (\c n -> down' 5 c n) (+) 0
  = down' 5 (+) 0

let go [] = []
    go (x : xs) = (x + 1) : go xs
in go ys

{-# RULES
"let/let" forall f g xs. -- ILLEGAL!
  let { x = let { y = e1 } in e2 } in e3
    = let { y = e1 } in let { x = e2 } in e3 #-}

{-# RULES
"unzip/map1" forall xs.
  unzip (map (\x -> (x, x)) xs) = (xs, xs) #-}

{-# RULES
"unzip/map2" forall a b xs.
  unzip (map (\x -> (a, b)) xs)
    = (map (\x -> a) xs, map (\x -> b) xs #-}

{-# RULES
"unzip/map3" forall f g xs.
  unzip (map (\x -> (f x, g x)) xs)
    = (map f xs, map g xs) #-}

{-# RULES "foo" forall f. foo (\y -> f y) = "RULE FIRED" #-}

data Stream a where
  Stream :: (s -> Step a s) -> s -> Stream a

data Step a s = Yield a s | Skip s | Done

stream :: [a] -> Stream a
stream xs = Stream uncons xs
  where uncons :: [a] -> Step a [a]
        uncons []       = Done
        uncons (x : xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream g s) = go s
  where go s = case g s of
                 Done       -> []
                 Skip s'    -> go s'
                 Yield x s' -> x : go s'

map :: (a -> b) -> [a] -> [b]
map f = unstream . mapS f . stream

mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream g s0) = Stream mapStep s0
  where mapStep s = case g s of
                      Done       -> Done
                      Skip s'    -> Skip s'
                      Yield x s' -> Yield (f x) s'

{-# RULES "stream/unstream"  (.) stream unstream = id #-}

x = map f . map g
  = unstream . mapS f . stream . unstream . mapS g . stream
  = unstream . mapS f . mapS g . stream -- opts
  = let go []       = []
        go (x : xs) = f (g x) : go xs
    in go

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = unstream . concatMapS (stream . f) . stream

concatMapS :: (a -> Stream b) -> Stream a -> Stream b
concatMapS f (Stream g s) = Stream g' (s, Nothing)
 where
  g' (s, Nothing) = case g s of
      Done       -> Done
      Skip s'    -> Skip (s', Nothing)
      Yield x s' -> Skip (s', Just (f x))
  g' (s, Just (Stream g'' s'')) = case g'' s'' of
      Done       -> Skip    (s, Nothing)
      Skip s'    -> Skip    (s, Just (Stream g'' s'))
      Yield x s' -> Yield x (s, Just (Stream g'' s'))

concatMapS' :: (a -> s -> Step s b) -> (a -> s) -> Stream a -> Stream b

{-# RULES
"concatMapS" forall next f.
  concatMapS (\x -> Stream (next x) (f x))
    = concatMapS' next f #-}

{-# RULES "map" forall f xs.
  map f xs = build (\c n -> foldr (mapFB c f) n xs) #-}

{-# RULES "mapList" forall f.
  foldr (mapFB (:) f) [] = map f #-}

{-# RULES "mapList2" forall f.
  foldr (\x xs -> f x : xs) [] = map f #-}
