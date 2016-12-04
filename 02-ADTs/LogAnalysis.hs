{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read

isInteger :: String -> Bool
isInteger str = case readMaybe str :: Maybe Integer of
                  Nothing -> False
                  Just _ -> True

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("I":time:content) | isInteger time  -> LogMessage Info (read time) (unwords content)
  ("W":time:content) | isInteger time -> LogMessage Warning (read time) (unwords content)
  ("E":level:time:content) | isInteger level && isInteger time -> LogMessage (Error (read level)) (read time) (unwords content)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time1 _) (Node left root@(LogMessage _ time2 _) right)
  | time1 < time2 = Node (insert msg left) root right
  | otherwise = Node left root (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getContent . inOrder . build . filter errorFilter 

errorFilter :: LogMessage -> Bool
errorFilter (LogMessage (Error level) _ _) = level >= 50
errorFilter _ = False

getContent :: LogMessage -> String
getContent (LogMessage _ _ content) = content

