{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module HWSix.Fibonacci (

)where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Lazy Loads infinite list of fib nums
fibs1 :: [Integer]
fibs1 = map fib [0..]

addFirstTwo :: [Integer] -> [Integer]
addFIrstTwo [x]          = x : x : []
addFirstTwo f@(x1:x2:xs) = (x1 + x2) : f
addFirstTwo _            = [1,0]

-- O(n) runtime version of prev funct
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

testFibs2 :: Int -> [Integer]
testFibs2 n = take n fibs2

--- Exercise Three

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons y c) = y : streamToList c

--- Exercise Four

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y c) = Cons (f y) $ streamMap f c

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

--- Exercise Five

nats :: Stream Integer 
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 c1) (Cons x2 c2) = Cons x1 (Cons x2 (interleaveStreams c1 c2))

ruler :: Stream Integer
ruler = startRuler 0

startRuler :: Integer -> Stream Integer
startRuler y = interleaveStreams (streamRepeat y) (startRuler (y+1))

--- Exercise 6

-- Represents generating function
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n                   = Cons n (streamRepeat 0)
  negate                          = streamMap (* (-1))
  (+) (Cons x1 c1) (Cons x2 c2)   = Cons (x1 + x2) ((+) c1 c2)
  (*) (Cons x1 c1) b@(Cons x2 c2) = Cons (x1 * x2) ((streamMap (*x1) c2) + ((*) c1 b))

instance Fractional (Stream Integer) where
  (/) a@(Cons x1 c1) b@(Cons x2 c2) = Cons (x1 `div` x2) (streamMap (`div` x2) (c1 - ((a / b)*c2)))

-- Uses Generating Functions to Define Closed Form Solution for Fib Nums
fib3 :: Stream Integer
fib3 = (x /) (1 - x - x^2)

--- Exercise 7

type Matrix = ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
  fromInteger n = ((n,0), (0,n))
  (*) ((a1,b1),(c1,d1)) ((a2,b2),(c2,d2)) = ( ((a1*a2 + b1*c2), (a1*b2 + b1*d2)) , ((c1*a2 + d1*c2), (c1*b2 + d1*d2)) )
  (+) ((a1,b1),(c1,d1)) ((a2,b2),(c2,d2)) = ( (a1 + a2 , b1 + b2) , (c1 + c2, d1 + d2) )

f :: Matrix
f = ((1,1) , (1,0))

--fib4 :: Integer -> Integer
-- log time fib generator
fib4 0 = 0
fib4 n = fst . fst $ f ^ (n - 1)
