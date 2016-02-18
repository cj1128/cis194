xor :: [Bool] -> Bool
xor = foldl (\v b -> if b then not v else v) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\c t -> (f c) : t) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = base

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 
  let s = takeWhile (<= n) (series n)
  in map (\n -> 2 * n + 1) $ filter (\n -> not (elem n s)) [1..n] 

series :: Integer -> [Integer]
series n = [i+j+2*i*j | i <- [1..n], j <- [i..n], i+j+2*i*j <= n]

