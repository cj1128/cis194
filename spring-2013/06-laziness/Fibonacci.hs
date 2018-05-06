{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
-- fib2 :: Integer -> Integer
-- fib2 n = iterate 1 0 1
--   where iterate index prev prev2
--           | index < n = iterate (index+1) (prev+prev2) prev
--           | otherwise = prev + prev2

-- fibs2 :: [Integer]
-- fibs2 = map fib2 [0..]
fibs2 :: [Integer]
fibs2 = map fn [0..] where
  fn 0 = 0
  fn 1 = 1
  fn k = fibs2 !! (k-1) + fibs2 !! (k-2)

-- exercise 3
data Stream a = Cons a (Stream a) deriving (Eq)

instance Show a => Show (Stream a) where
  show = (++"...") . show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- this is not the best implementation, but it works
-- ruler :: Stream Integer
-- ruler = streamMap nthRuler (streamFromSeed (+1) 1)

-- nthRuler :: Integer -> Integer
-- nthRuler x = head . reverse $ takeWhile (\n -> mod x (2^n) == 0 && x >= n) [0..]

ruler :: Stream Integer
ruler = streamMap fn (streamFromSeed (+1) 1) where
  fn k | odd k = 0
       | even k = 1 + (streamToList ruler !! (k `div` 2 - 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) s = Cons x $ interleaveStreams s xs

ruler2 :: Stream Integer
ruler2 = interleaveStreams (streamRepeat 0) (streamMap rulerValue $ streamFromSeed (+2) 2) where
  rulerValue n | odd n = 0
               | otherwise = 1 + rulerValue (n `div` 2)

-- exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate s = streamMap (*(-1)) s
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) $ (+) xs ys
  (*) (Cons x xs) yStream@(Cons y ys) = Cons (x*y) $ streamMap (*x) ys + xs * yStream

instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) = let q = Cons (x `div` y) $ streamMap (`div` y) (xs - q * ys) in q

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- exercise 7
data Matrix = Matrix Integer Integer Integer Integer deriving (Eq, Show)

instance Num Matrix where
  (*) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) = Matrix (x1*y1 + x2*y3) (x1*y2 + x2*y4) (x3*y1+x4*y3) (x3*y2+x4*y4)

f :: Matrix
f = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = get $ f^(n-1) where
  get (Matrix x1 _ _ _) = x1
