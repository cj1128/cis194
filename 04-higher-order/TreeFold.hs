data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree value Leaf = Node 0 Leaf value Leaf

insertTree value (Node height left nodeValue right)
  | nodeHeight left < nodeHeight right = 
    let newLeft = insertTree value left
    in Node (maximum [(nodeHeight newLeft) + 1, height]) newLeft nodeValue right
  | otherwise = 
    let newRight = insertTree value right
    in Node (maximum [(nodeHeight newRight) + 1, height]) left nodeValue newRight

nodeHeight :: Tree a -> Integer
nodeHeight (Node height _ _ _) = height
nodeHeight otherwise = -1


