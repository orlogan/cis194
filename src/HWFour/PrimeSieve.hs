{-# OPTIONS_GHC -Wall #-}

module HWFour.PrimeSieve (

)where

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x + 1) . sieveFilter

-- Takes integer and gives list of all 
-- ints from 1 .. n not filtered by Sundaram
-- i.e i + j + 2ij <= n
sieveFilter :: Integer -> [Integer]
sieveFilter n = [1 .. n] \\ [(x + y + 2*x*y) | x <- [1 .. n], y <- [1 .. n], x <= y, (x + y + 2*x*y) <= n]
