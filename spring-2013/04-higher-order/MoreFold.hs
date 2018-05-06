xor :: [Bool] -> Bool
xor = foldl (\v b -> if b then not v else v) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\c t -> (f c) : t) []

-- https://wiki.haskell.org/Foldl_as_foldr_alternative
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base list = (foldr construct (\acc -> acc) list) base
  where
    construct x r = \acc -> r (f acc x)

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f base = foldr (flip f) base . reverse

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (not . (`elem` (series n))) [1..n]

series :: Integer -> [Integer]
series n = [i+j+2*i*j | i <- [1..n], j <- [i..n], i+j+2*i*j <= n]

