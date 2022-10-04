{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HWEight.Party
  ( glCons,
    moreFun,
  )
where

import HWEight.Employee

-- We may assume no weird cases hold,
-- so just add the employee to the list and
-- add their fun score to the aggregate score
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL es f) = GL (e : es) (f + ef)

instance Semigroup GuestList where
  (<>) (GL em1s f1) (GL em2s f2) = GL (em1s ++ em2s) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 < f2 = gl2
  | otherwise = gl1


-- Test GL's
glOne :: GuestList
glOne = GL [Emp "Bob" 3, Emp "Beb" 10,Emp "Boob" 1] 14

glTwo :: GuestList
glTwo = GL [Emp "Dirk" 3, Emp "Derek" 2,Emp "Darnell" 7] 12
