module Main where

import HWSeven.JoinListBuffer
import HWSeven.Editor

main :: IO()
main = runEditor editor . createList $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]


-- https://www.cis.upenn.edu/~cis1940/spring13/lectures.html
-- https://github.com/bschwb/cis194-solutions/blob/main/07-folds-monoids/Sized.hs
