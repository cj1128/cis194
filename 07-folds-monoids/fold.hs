data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf a = Node Empty a Empty


treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = (flatten l) ++ [x] ++ (flatten r)

testTree :: Tree Integer
testTree = Node 
            (Node 
              (Node (leaf 100) 200 (leaf 300))
              2
              (leaf 3))
            4
            (Node (leaf 5) 6 (leaf 7))

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)


treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> l + r + 1)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

