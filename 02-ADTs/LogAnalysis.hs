{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("I":time:content) -> LogMessage Info (read time) (unwords content)
  ("W":time:content) -> LogMessage Warning (read time) (unwords content)
  ("E":level:time:content) -> LogMessage (Error (read level)) (read time) (unwords content)
  otherwise -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert log@(LogMessage _ time1 _) (Node left root@(LogMessage _ time2 _) right)
  | time1 < time2 = Node (insert log left) root right
  | otherwise = Node left root (insert log right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getContent . inOrder . build . filter errorFilter 

errorFilter :: LogMessage -> Bool
errorFilter (LogMessage (Error level) _ _)
  | level >= 50 = True
  | otherwise = False
errorFilter otherwise = False

getContent :: LogMessage -> String
getContent (LogMessage _ _ content) = content

