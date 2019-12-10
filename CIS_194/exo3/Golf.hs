module Golf where

 import Data.IntMap.Strict (IntMap)
 import qualified Data.IntMap.Strict as IntMap

-- Ex 1
skip :: Int -> [a] -> [a]
skip _ []                   = []
skip n l@(x:[]) | n <= 1    = l
                     | otherwise = []
skip n l = case (drop (n-1) l) of
             []      -> []
             (x:[])  -> [x]
             (x:xs)  -> [x] ++ (skip n xs)


skipsUntil :: Int -> [a] -> [[a]]
skipsUntil n l 
          | n <= 1        = [l]
          | n > length(l) = []
          | otherwise     = case l of
                               []     -> []
                               _      -> (skipsUntil (n - 1) l) ++ [(skip n l)] 



skips :: [a] -> [[a]]
skips x      = skipsUntil (length x) x

-- Ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima []         = []
localMaxima (x:[])     = []
localMaxima (x:y:[])   = []
localMaxima (x:y:z:xs) | (x < y && y > z) = y : localMaxima (y:(z:xs))
                       | otherwise        = localMaxima (y:(z:xs))

-- Ex 3
countNumbers :: [Integer] -> IntMap

histogram :: [Integer] -> String
