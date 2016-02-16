type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end temporary
  | n == 1 = [(start, end)]
  | otherwise = hanoi (n - 1) start temporary end ++ [(start, end)] ++ hanoi (n - 1) temporary end start

