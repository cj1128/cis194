{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where
import ExprT 
import Parser
import qualified StackVM as VM
import qualified Data.Map as M

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

instance Expr VM.Program where
  lit n = [VM.PushI n] 
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer | VAdd VarExprT VarExprT | VMul VarExprT VarExprT | Var String

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var key = \map ->
    M.lookup key map 

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = \_ -> Just n 
  add x y = \map ->
    let v1 = x map
        v2 = y map
    in case (v1, v2) of
      (Just m, Just n) -> Just (m + n)
      _ -> Nothing
  mul x y = \map ->
    let v1 = x map
        v2 = y map
    in case (v1, v2) of
      (Just m, Just n) -> Just (m * n)
      _ -> Nothing

withVars :: [(String , Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vars exp = exp $ M.fromList vars
