{-# OPTIONS_GHC -Wall #-}
module Golf where
import Data.List
import qualified Data.Map as M

skips :: [a] -> [[a]]
skips l = map everyN  [1..length l] where
  everyN n = map (\x -> l !! (x-1)) $ filter (\x -> x `mod` n == 0) [1..length l]

localMaximum :: [Integer] -> [Integer]
localMaximum l = map (\(_, y ,_) -> y) . filter (\(x, y, z) -> y > x && y > z) $ zip3 l (drop 1 l) (drop 2 l)

histogram :: [Integer] -> String
histogram = drawHistogram . transformData
--
transformData :: [Integer] -> M.Map Integer Integer
transformData = foldr (\n acc -> M.insertWith (+) n 1 acc) M.empty
--
-- input is a map, represents frequency for 0-9
drawHistogram :: M.Map Integer Integer -> String
drawHistogram h = (++ "==========\n0123456789\n") . unlines . transpose $ map fn [0..9] where
  maxValue = maximum $ M.elems h
  fn k = case M.lookup k h of
           Nothing -> genColumn maxValue 0
           Just x -> genColumn maxValue x

-- generate a column based on max length and current value,
-- e.g. genColumn 4 1 -> "   *"
genColumn :: Integer -> Integer -> String
genColumn maxLength value = replicate (fromIntegral $ maxLength - value) ' ' ++ replicate (fromIntegral value) '*'
