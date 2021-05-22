{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool (..), Enum (succ), Eq, Foldable (null, sum), Fractional ((/)), IO (..), Int, Integral (div, mod), Maybe (..), Num ((*), (+), (-)), Ord ((>)), all, concat, const, even, flip, fst, head, id, not, odd, otherwise, putStrLn, reverse, scanr, tail, undefined, zip, ($), (&&), (++), (.), (||))

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
unwrap (x : xs) = x

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
filter2 pred = foldr (\x acc -> if pred x then x : acc else acc) []

-- 1.6 express foldr f e . filter p as an instance of foldr

abc f p e = foldr f e . filter p

abc2 f p = foldr op
  where
    op x acc = if p x then f x acc else acc

-- 1.7

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f e [] = e
-- foldr f e (x : xs) = f x (foldr f e xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr op []
  where
    op x acc = if f x then x : acc else []

-- 1.8

-- dropWhileEnd even [1, 4, 3, 6, 2, 4] = [1, 4, 3]

-- https://stackoverflow.com/questions/27456313/how-to-implement-delete-with-foldr-in-haskell
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd f xs = foldr op (const []) xs xs
  where
    -- op :: a -> ([a] -> [a]) -> ([a] -> [a])
    op x g state =
      if all f state
        then g []
        else x : g (tail state)

-- op rest x acc = if all f xs then x : acc else []

dropWhileEnd2 :: (a -> Bool) -> [a] -> [a]
dropWhileEnd2 p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- delete :: Eq a => a -> [a] -> [a]
-- delete a xs = foldr f _ xs _
--   where
--     f :: a -> (Bool -> [a]) -> (Bool -> [a])
--     f x g = _

-- 1.9

-- foldr f e xs = if null xs then e else f (head xs) (foldr f e (tail xs))
-- foldl f e xs = if null xs then e else f (foldl f e (init xs)) (last xs)

-- init and last are O(n) which makes the foldl defined above as very inefficient

-- 1.10

-- when is foldr op e xs = foldl op e xs ?

-- when op is associative with an identity element

-- 1.11

-- write integer [1, 4, 5, 6] = 1456
-- wrte fraction [1, 4, 6, 7] = 0.1467

integer = foldl op 0
  where
    op x 0 = x
    op x acc = 10 * x + acc

fraction = foldr op 0
  where
    op x 0 = x / 10
    op x acc = x / 10 + acc / 10

-- 1.12

-- complete the RHS of
-- map (foldl f e) . init = scanl f e
-- map (foldr f e) . tail = scanr f e

-- Master Rule
-- h (foldr f e xs) = foldr g (h e) xs
-- h . foldr f e = foldr g (h e)
-- Fusion Condition
-- h (f x y) = g x (h y)
-- f :: a -> b -> b
-- h :: b -> b
-- g :: a -> b -> b

-- h = map
-- foldr g (map e) xs

-- 1.13

apply :: Nat -> (a -> a) -> a -> a
apply 0 _ v = v
apply n f v = apply (n - 1) f (f v)

apply2 0 f = id
apply2 n f = f . apply2 (n - 1) f


-- 1.14
