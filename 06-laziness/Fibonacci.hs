module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- fast fibonacci
fib2 :: Integer -> Integer
fib2 n = iterate 1 0 1
  where iterate index prev prev2
          | index < n = iterate (index+1) (prev+prev2) prev
          | otherwise = prev + prev2

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fib2 [0..]

data Stream a = Cons a (Stream a) deriving (Eq)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = (++"...") . show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- this is not the best implementation, but it works
ruler :: Stream Integer
ruler = streamMap nthRuler (streamFromSeed (+1) 1)

nthRuler :: Integer -> Integer
nthRuler x = head . reverse $ takeWhile (\n -> mod x (2^n) == 0 && x >= n) [0..]

