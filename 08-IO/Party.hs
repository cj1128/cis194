module Party where
import Employee
import Data.Tree
import Data.List(sort)

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps f) = GL (emp:emps) (f + (empFun emp)) 

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emps1 f1) (GL emps2 f2) = GL (emps1 ++ emps2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) 
  | f1 > f2 = gl1
  | otherwise = gl2


getFun :: GuestList -> Fun
getFun (GL _ f) = f

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v subs) = f v (map (treeFold f) subs)

nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel boss subs = let
  withBoss = glCons boss (mconcat $ map snd subs)
  withoutBoss = mconcat $ map fst subs
  in (withBoss, withoutBoss)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

formatList :: GuestList -> String
formatList (GL emps fun) = 
  let firstLine = "Total fun: " ++ show fun
      names = sort $ map empName emps
  in unlines $ firstLine:names

main :: IO ()
main = readFile "./company.txt" >>= putStrLn . formatList . maxFun . read
