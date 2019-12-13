{-#       OPTIONS_GHC -fno-warn-orphans #-}

import Employee
import Data.Tree
import Data.List

-- Ex 1
-- 1.
getFun :: Employee -> Fun
getFun (Emp _ fun) = fun

glCons :: Employee -> GuestList -> GuestList
glCons emp gl@(GL empList totalFun) = GL (emp : empList) (totalFun + getFun emp)

-- 2.
instance Monoid GuestList where
   mempty = GL [] 0

instance Semigroup GuestList where
   (<>) (GL emplList1 totalFun1) (GL emplList2 totalFun2) = GL (emplList1 ++ emplList2) (totalFun1 + totalFun2)

-- 3.
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ totalFun1) gl2@(GL _ totalFun2) | totalFun2 > totalFun1 = gl2
                                                  | otherwise             = gl1

-- Ex 2
treeFold :: [b] -> (a -> [b] -> b) -> Tree a -> b
treeFold k f (Node rootLabel []) = f rootLabel k
treeFold k f (Node rootLabel subForest) = f rootLabel (map (treeFold k f) subForest)

-- Ex 3
nextLevel :: Employee -> [(GuestList, GuestList)]-> (GuestList, GuestList)
nextLevel boss gl = ((GL [boss] (getFun boss)), mconcat $ map (uncurry moreFun) gl)

-- Ex 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . (treeFold mempty nextLevel)

-- Ex 5
main :: IO ()
main = do
  empList <- readFile "company.txt"
  putStrLn . formatGL . maxFun . read $ empList

formatGL :: GuestList -> String
formatGL (GL empList fun) =
  "Total fun: " ++ show fun ++ "\n" ++ list
  where list = unlines. sort . map empName $ empList
