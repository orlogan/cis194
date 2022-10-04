{-# OPTIONS_GHC -Wall #-}

module HWThree.Golf where 

hop :: [a] -> Int -> [a]
hop str n = map fst $ filter (\x -> snd x `mod` n == 0) (zip str [1..]) 

skips :: [a] -> [[a]]
skips str = map (hop str) [1 .. length(str)]

-- localMax :: [Integer] -> [Integer]
--
-- localMin :: [Integer] -> [Integer]

checkMax :: Integer -> Integer -> Integer -> [Integer]
checkMax a b c 
  | a < b && b > c  = [b]
  | otherwise       = []

checkMin :: Integer -> Integer -> Integer -> [Integer]
checkMin a b c 
  | a > b && b < c  = [b]
  | otherwise       = []


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = (checkMax a b c) ++ (checkMin a b c) ++ localMaxima (b:c:xs)
localMaxima _          = []
