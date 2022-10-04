{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HWSeven.JoinListBuffer where

import Data.Monoid ()
import Data.String

import HWSeven.Buffer
import HWSeven.JoinList
import HWSeven.Sized
import HWSeven.Scrabble

stringValue :: Monoid m => JoinList m String -> String
stringValue (Single _ a)       = a
stringValue (Append _ jl1 jl2) = stringValue jl1 ++ stringValue jl2
stringValue _                  = ""



instance Buffer (JoinList (Score, Size) String) where
  toString       = stringValue
  fromString str = foldr (+++) Empty $ map (\x -> Single (scoreString x, 1) x) (lines str) 
  line           = indexJ
  replaceLine _ _ Empty          = Empty
  replaceLine _ str (Single _ _) = HWSeven.Buffer.fromString str
  replaceLine n str jl@(Append s jl1 jl2)
    | n < 0       = jl
    | n >= sizeS  = jl
    | n < s1      = Append s (replaceLine n str jl1) jl2
    | otherwise   = Append s jl1 (replaceLine (n - s1) str jl2)
    where sizeS  = getSize $ size s
          s1     = getSize . size $ tag jl1
  numLines Empty          = 0 
  numLines (Single _ _)   = 1
  numLines (Append s _ _) = getSize $ size s 
  value Empty                 = 0 
  value (Single (v,_) _)      = getScore v
  value (Append (v,_) _ _)    = getScore v




-- Testing Stuff

createList :: String -> JoinList (Score, Size) String
createList str = foldr (+++) Empty $ map (\x -> Single (scoreString x, 1) x) (lines str) 
