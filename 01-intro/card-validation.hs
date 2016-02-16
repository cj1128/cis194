import Data.Char

toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0 = []
  | otherwise = map (toInteger . digitToInt) $ show i

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:ys) = x:(y*2):(doubleEveryOtherFromLeft ys)

sumDigits :: [Integer] -> Integer
sumDigits = sum . map digitsValue

digitsValue :: Integer -> Integer
digitsValue = sum . toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
