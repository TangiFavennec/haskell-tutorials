-- Ex 1
-- 1.fun1 :: [Integer] -> Integer
--   fun1 []     = 1
--   fun1 (x:xs)| even x    = (x - 2)*fun1 xs
--              | otherwise = fun1 xs
-- 2.fun2 :: Integer -> Integer
--   fun2 1 = 0
--   fun2 n | even n    = n + fun2 (n ‘div‘ 2)
--          | otherwise = fun2 (3*n + 1)

fun1 :: [Integer] -> Integer
fun1 =  (foldr (\x y -> (x-2) * y) 1) . filter (even)

fun2Seq :: Integer -> Integer
fun2Seq x | x `mod` 2 == 0 = x `div` 2
      | otherwise      = 3 * x + 1

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate fun2Seq

-- Ex 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

unitInsert :: Integer -> a -> Tree a
unitInsert h val = Node h Leaf val Leaf

getHeight :: Tree a -> Integer
getHeight Leaf = (-1)
getHeight (Node _ l _ r) = 1 + max (getHeight l) (getHeight r)

insert :: a -> Tree a -> Tree a
insert x Leaf                         = unitInsert 0 x
insert x (Node height Leaf val Leaf)  = Node (height + 1) (insert x Leaf) val Leaf
insert x (Node height l val r)
       | (getHeight l == getHeight r) = Node (getHeight insertLeft + 1) insertLeft val r
       | (getHeight l > getHeight r)  = Node height l val insertRight
       | otherwise                    = Node height insertLeft val r
  where insertLeft  = insert x l
        insertRight = insert x r

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Ex 3
-- 1.
unitXor :: Bool -> Bool -> Bool
unitXor False True  = True
unitXor True  False = True
unitXor _ _         = False

xor :: [Bool] -> Bool
xor = foldr unitXor False

-- 2.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y ) []

-- 3.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =  foldr (\x y -> f y x) base (reverse xs)

-- Ex 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [x*2 + 1 | x <- [1..(2*n + 1)],
                          not (elem x [i + j + 2 * i * j | i <- [1..n], j <- [1..n]])]