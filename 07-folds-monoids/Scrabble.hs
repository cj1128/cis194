{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import qualified Data.Map as M
import Data.Char

newtype Score = Score Int
  deriving (Eq, Show, Num, Ord)

instance Monoid Score where
  mempty = 0
  mappend = (+)


score :: Char -> Score
score c = case M.lookup (toUpper c) $ M.fromList scrabble of
  Just s -> Score s
  Nothing -> 0

scrabble :: [(Char, Int)]
scrabble =
  [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1)
  ,('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8)
  ,('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1)
  ,('P', 3), ('Q', 10), ('R', 1), ('S', 1), ('T', 1)
  ,('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4)
  ,('Z', 10)]

scoreString :: String -> Score
scoreString = mconcat . map score
