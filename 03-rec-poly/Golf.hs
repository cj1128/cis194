module Golf where
import Data.List

skips :: [a] -> [[a]]
skips l = map (chunk l) [1..(length l)]

-- return a list of every other nth element
chunk :: [a] -> Int -> [a]
chunk l n
  | n <= 0 = []
  | n > length l = []
  | otherwise = last (take n l) : chunk (drop n l) n



localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(x:y:z) -> y) . filter (\(x:y:z:zs) -> y > x && y > z) . groupBy3

-- return groups with 3 items
groupBy3 :: [Integer] -> [[Integer]]
groupBy3 l
  | length l < 3 = []
  | otherwise = (take 3 l) : groupBy3 (tail l)

histogram :: [Integer] -> String
histogram l = unlines $ drawHistogram (map (count l) [0..9]) ++ ["==========", "0123456789"]

count :: [Integer] -> Integer -> Integer
count l t = fromIntegral $ length $ filter (== t) l

drawHistogram :: [Integer] -> [String]
drawHistogram l = let max = maximum l
                      f = (`replicate` '*')
                  in transpose $ map (pad max . f . fromIntegral) l

-- pad string to length 10 with leading space
pad :: Integer -> String -> String
pad n s
  | length s < fromIntegral n = pad n (' ':s)
  | otherwise = s

