{-# OPTIONS_GHC -Wall #-}

module HWFour.HW4 (

)where

-- Multiplies all even numbers less 2
fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (\x -> x - 2) . filter even

-- Collatz conjecture
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y) ) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> [f(x)] ++ y) [] 
