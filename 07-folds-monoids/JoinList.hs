{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where
import Sized
import Scrabble
import Data.Monoid
import Editor
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l `mappend` tag r) l r

testJoinList :: JoinList Size Char
testJoinList = ((Single 1 'y') +++
  ((Single 1 'e') +++ (Single 1 'a')))
  +++ (Single 1 'h')

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing
indexJ _ Empty = Nothing
indexJ n (Single m a)
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append m left right)
  | n >= getSize (size m) = Nothing
  | n >= leftSize = indexJ (n - leftSize) right
  | otherwise = indexJ n left
  where leftSize = getSize . size . tag $ left

testIndexJ :: Int -> Bool
testIndexJ i = (indexJ i testJoinList) == (jlToList testJoinList !!? i)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

dropJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i < 1 = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append _ l r) =
  let leftSize = getSize $ size (tag l)
  in if i > leftSize then
    dropJ (i - leftSize) r
  else
    (dropJ i l) +++ r

testDropJ :: Int -> Bool
testDropJ n = jlToList (dropJ n testJoinList) == drop n (jlToList testJoinList)

takeJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ l r) =
  let leftSize = getSize $ size (tag l)
  in if n > leftSize then
    l +++ takeJ (n - leftSize) r
  else if n == leftSize then
    l
  else
    takeJ n l


testTakeJ :: Int -> Bool
testTakeJ n = jlToList (takeJ n testJoinList) == take n (jlToList testJoinList)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = construct . lines
  line = indexJ
  -- after replace, the tree is not balanced
  replaceLine n l b =
    takeJ n b +++ insertJoinList l (dropJ (n+1) b)
  numLines = length . jlToList
  value = getScore . fst . tag

single :: String -> JoinList(Score, Size) String
single str = Single (scoreString str, 1) str

-- construct a balanced JoinList each stores a line
-- we need to keep the order
-- jlToList $ construct lines == lines
construct :: [String] -> JoinList (Score, Size) String
construct = foldr insertJoinList Empty

insertJoinList :: String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
insertJoinList line Empty = single line
insertJoinList line jl@(Single m a) = single line +++ jl
insertJoinList line jl@(Append m l r) =
  if mod (getSize . size $ tag jl) 2 == 0
  then
    single line +++ jl
  else
    insertJoinList line l +++ r

main = runEditor editor $ construct
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
