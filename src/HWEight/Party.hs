{-# OPTIONS_GHC -Wall #-}

module HWEight.Party where

import HWEight.Employee

-- We may assume no weird cases hold,
-- so just add the employee to the list and
-- add their fun score to the aggregate score
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL es f) = GL (e : es) (f + ef)
