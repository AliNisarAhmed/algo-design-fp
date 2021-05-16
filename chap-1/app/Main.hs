{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool(..), Enum (succ), Foldable (null, sum), IO (..), Int, Maybe(..), Num ((+)), Ord ((>)), concat, flip, head, id, not, putStrLn, reverse, tail, zip, ($), (++), (.), (||))

main :: IO ()
main = putStrLn "Hello, Haskell!"

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) = if f x then x : filter f xs else filter f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e [] = e
foldr f e (x : xs) = f x (foldr f e xs)

type Nat = Int

label :: [a] -> [(Nat, a)]
label xs = zip [0 ..] xs

length :: [a] -> Nat
length = foldr succ 0
  where
    succ _ n = n + 1

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e [] = e
foldl f e (x : xs) = foldl f (f e x) xs

foldl2 f e = foldr (flip f) e . reverse

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f e [] = []
scanl f e (x : xs) = e : scanl f (f e x) xs

---

perms1 :: [a] -> [[a]]
perms1 [] = [[]]
perms1 (x : xs) = [zs | ys <- perms1 xs, zs <- inserts x ys]

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y : ys) = (x : y : ys) : map (y :) (inserts x ys)

---

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

perms1' :: [a] -> [[a]]
perms1' = foldr step [[]]
  where
    step x xss = concatMap (inserts x) xss

-- step = concatMap . insert

perms1'' :: [a] -> [[a]]
perms1'' = foldr (concatMap . inserts) [[]]

---

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [x : zs | (x, ys) <- picks xs, zs <- perms2 ys]

-- The function picks picks an arbitrary element from a list in all possible ways, returning both the element and what remains
picks :: [a] -> [(a, [a])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

perms2' [] = [[]]
perms2' (x : xs) = concatMap subperms (picks xs)
  where
    subperms (x, ys) = map (x :) (perms2' ys)

---

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

while :: (a -> Bool) -> (a -> a) -> a -> a
while p = until (not . p)

-- Fusion

-- map f . map g = map (f . g)
-- concatMap f . map g = concatMap (f . g)
-- foldr f e . map g = foldr (f . g) e

x :: (a -> c -> c) -> c -> [[a]] -> c
x f e = foldr f e . concat

y :: (a -> c -> c) -> c -> [[a]] -> c
y f = foldr (flip (foldr f))

d1 f e xs ys = foldr f e (xs ++ ys)

d2 f e xs ys = foldr f (foldr f e ys) xs

-- Master Rule
-- h (foldr f e xs) = foldr g (h e) xs
-- h . foldr f e = foldr g (h e)
-- Fusion Condition
-- h (f x y) = g x (h y)
-- f :: a -> b -> b
-- h :: b -> b
-- g :: a -> b -> b

------------------------------------

collapse :: [[Int]] -> [Int]
collapse = help []

help xs xss =
  if sum xs > 0 || null xss
    then xs
    else help (xs ++ head xss) (tail xss)

collapse2 :: [[Int]] -> [Int]
collapse2 xss = help2 (0, []) (labelsum xss)

labelsum :: [[Int]] -> [(Int, [Int])]
labelsum xss = zip (map sum xss) xss

help2 :: (Int, [Int]) -> [(Int, [Int])] -> [Int]
help2 (s, xs) xss =
  if s > 0 || null xss
    then xs
    else help2 (cat (s, xs) (head xss)) (tail xss)

cat :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
cat (s, xs) (t, ys) = (s + t, xs ++ ys)

---
-- concatenating from right to left for better performance instead of directly (++)
collapse3 xss = (help3 (0, id) (labelsum xss)) []

help3 (s, f) xss =
  if s > 0 || null xss
    then f
    else help3 (s + t, f . (xss ++)) (tail xss)
  where
    (t, xs) = head xss

---- Exercises

-- 1.2

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

-- 1.3

wrap :: a -> [a]
wrap x = [x]

unwrap :: [a] -> a
unwrap (x:xs) = x

single :: [a] -> Bool
single [x] = True
single _ = False


-- 1.4

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []


-- 1.5

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr ((:) . f) []

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 pred = foldr (\x acc -> if pred x then x:acc else acc) []