{-# OPTIONS_GHC -Wall #-}

-- https://www.cis.upenn.edu/~cis1940/spring13/lectures.html

module HWOne.CreditCard (
validate
) where

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []

reverseDouble :: [Integer] -> [Integer]
reverseDouble (x:y:xs) = reverseDouble xs ++ 2*y : [x]
reverseDouble [x]      = [x]
reverseDouble _        = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverseDouble (reverse x)

-- sum digits of integers, i.e. each numberplace
sumDigits :: [Integer] -> Integer
sumDigits (x:xs) 
  | x > 10     = sumDigits (xs ++ toDigits x)
  | otherwise  = x + sumDigits xs
sumDigits []   = 0

checkSum :: Integer -> Integer
checkSum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate n
  | (checkSum n) `rem` 10 == 0 = True
  | otherwise                  = False
