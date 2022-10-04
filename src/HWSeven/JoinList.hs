{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}


module HWSeven.JoinList where

--import Data.Monoid
import HWSeven.Sized
import HWSeven.Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
tag _              = mempty


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2 

indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ _ (Single _ a) = Just a
indexJ n (Append s jl1 jl2)
  | n < 0       = Nothing
  | n >= sizeS  = Nothing
  | n < s1      = indexJ n jl1
  | otherwise   = indexJ (n - s1) jl2
  where sizeS  = getSize $ size s
        s1     = getSize . size $ tag jl1




dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ (Single _ _) jl2) = dropJ (n - 1) jl2
dropJ n (Append _ (Empty) jl2) = dropJ n jl2
dropJ n (Append s jl1@(Append _ _ _) jl2)
  | n >= sizeS = Empty 
  | n < s1     = (dropJ n jl1) +++ jl2
  | otherwise  = dropJ (n - s1) jl2
  where sizeS  = getSize $ size s
        s1     = getSize . size $ tag jl1


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- For Testing

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

newtype Sum n = Sum n
  deriving (Eq, Ord, Show)

instance Num n => Semigroup (Sum n) where
  Sum x <> Sum y = Sum (x + y)

instance Num n => Monoid (Sum n) where
  mempty = Sum 0

newtype Product n = Product n
  deriving (Eq, Ord, Show)

instance Num n => Semigroup (Product n) where
  Product x <> Product y = Product (x * y)

instance Num n => Monoid (Product n) where
  mempty = Product 1

-- (Single (Size 1) 'A') +++ (Single (Size 1) 'B') +++ (Single (Size 1) 'C') +++ (Single (Size 1) 'D') +++ (Single (Size 1) 'E')
-- ( Append (Size 5) (Append (Size 3) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b') ) (Single (Size 1) 'c')) (Append (Size 2) (Single (Size 1) 'd') (Single (Size 1) 'e')))
