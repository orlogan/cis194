{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HWFive.Calc (

)where

import HWFive.ExprT
import HWFive.Parser
import Data.Maybe
import qualified Data.Map as M

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

class HasVars a where
  var :: String -> a

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a 

type MapExpr = M.Map String Integer -> Maybe Integer

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where 
  lit     = id
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x 
    | x <= 0   = False
    |otherwise = True
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit x   = MinMax x
  add x y 
    | x <= y    = x
    | otherwise = y
  mul x y 
    | x > y     = x
    | otherwise = y

instance Expr Mod7 where
  lit x   = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7) 

instance Expr MapExpr where 
  lit a = (\_ -> Just a)
  add f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                    True -> Nothing
                    _    -> Just (fromJust (f m) + fromJust (g m))
  mul f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                    True -> Nothing
                    _    -> Just (fromJust (f m) * fromJust (g m))

instance HasVars MapExpr where
  var = M.lookup

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul
  
instance HasVars VarExprT where
  var str = Var str

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n1 n2) = (eval n1) + (eval n2)
eval (Mul n1 n2) = (eval n1) * (eval n2)

checkEval :: Maybe ExprT -> Maybe Integer
checkEval (Just e)  = Just (eval e)
checkEval (Nothing) = Nothing

evalStr :: String -> Maybe Integer
evalStr = checkEval . (parseExp Lit Add Mul) 

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

withVars :: [(String, Integer)]
         -> MapExpr
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
