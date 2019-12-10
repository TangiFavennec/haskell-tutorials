{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser(parseExp)
import StackVM

-- Ex 1
eval :: ExprT -> Integer
eval (Lit x)       = x
eval (ExprT.Add x y) = (eval x) + (eval y) 
eval (ExprT.Mul x y) = (eval x) * (eval y) 

-- Ex 2
evalStr :: String -> Maybe Integer
evalStr s = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul s) of
              Just expr -> Just (eval expr)
              Nothing   -> Nothing

-- Ex 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a  -> a
    mul :: a -> a  -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Ex 4
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit b = if b <= 0 then False else True
    add   = (&&)
    mul   = (||)

instance Expr MinMax where
    lit                       = MinMax . id  
    add (MinMax a) (MinMax b) = MinMax (min a b)
    mul (MinMax a) (MinMax b) = MinMax (max a b)

instance Expr Mod7 where
    lit x                 = Mod7 (x `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

-- Ex 5
instance Expr Program where
   lit x   = [PushI x]
   add a b = a ++ b ++ [StackVM.Add]
   mul a b = a ++ b ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul 

-- Ex 6 (Later)