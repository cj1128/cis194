{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT 
import Parser
--import StackVM

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add left right) = (eval left) + (eval right)
eval (Mul left right) = (eval left) * (eval right)

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
  Just exprT -> Just (eval exprT)
  Nothing -> Nothing

class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul


instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n
    | n <= 0 = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit n = Mod7 n
  add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

{-
instance Expr Program where
  lit n = [PushI n] 
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
-}

