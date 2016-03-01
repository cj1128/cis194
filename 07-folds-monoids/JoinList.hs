module JoinList where
import Sized
import Scrabble

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
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l r) = 
  let leftSize = getSize $ size (tag l)
  in if i >= leftSize then
    indexJ (i - leftSize) r
  else
    indexJ i l

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
  else 
    takeJ n l


testTakeJ :: Int -> Bool
testTakeJ n = jlToList (takeJ n testJoinList) == take n (jlToList testJoinList)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str



