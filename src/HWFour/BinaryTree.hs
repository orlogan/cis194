{-# OPTIONS_GHC -Wall #-}
module HWFour.BinaryTree (
foldTree
)where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node n _ _ _) = n

insert :: a -> Tree a -> Tree a
insert x Leaf = (Node 1 Leaf x Leaf)
insert x (Node n t1 b t2) 
  | h1 < h2   = Node n (insert x t1) b t2    
  | h2 < h1   = Node n t1 b t2n
  | otherwise = Node (h + 1) t1 b t2n
  where h1  = height t1
        h2  = height t2
        t2n = insert x t2
        h   = height t2n

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf
