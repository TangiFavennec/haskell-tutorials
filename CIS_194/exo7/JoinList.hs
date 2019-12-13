{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Sized
import Buffer
import Scrabble
import Editor

-- Ex 1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) left right = Append ((tag left) <> (tag right)) left right

-- Ex 2
-- 1.
getIntSize :: Sized a => a -> Int
getIntSize = getSize . size

getIntSizeFromJoinList :: (Sized m, Monoid m) => JoinList m a -> Int
getIntSizeFromJoinList = getIntSize . tag

indexJ :: (Sized b, Monoid b) =>
       Int -> JoinList b a -> Maybe a
indexJ _ Empty                                           = Nothing
indexJ n (Single m val) | n == 0                         = Just val
                        | otherwise                      = Nothing

indexJ n (Append m l r) | n > (getIntSize m)             = Nothing
                        | n < (getIntSizeFromJoinList l) = indexJ (n - 1) l
                        | otherwise                      = indexJ (n - (getIntSizeFromJoinList l)) r

-- 2.
dropJ :: (Sized b, Monoid b) =>Int -> JoinList b a -> JoinList b a
dropJ _ Empty                                           = Empty
dropJ n jl@(Single _ _) | n <= 0                        = jl
                        | otherwise                     = Empty

dropJ n (Append m l r) | n > (getIntSize m)             = Empty
                       | n < (getIntSizeFromJoinList l) = dropJ (n - 1) l
                       | otherwise                      = dropJ (n - (getIntSizeFromJoinList l)) r

-- 3.
takeJ :: (Sized b, Monoid b) =>
      Int -> JoinList b a -> JoinList b a
takeJ _ Empty                                             = Empty
takeJ n jl@(Single _ _) | n <= 1                          = Empty
                        | otherwise                       = jl

takeJ n jl@(Append m l r) | n > (getIntSize m)            = jl 
                         | n < (getIntSizeFromJoinList l) = takeJ (n - 1) l
                         | otherwise                      = l +++ (takeJ (n - (getIntSizeFromJoinList l)) r)

-- For testing
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Ex 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Ex 4
instance Monoid Size where
    mempty = Size 0

instance Semigroup Size where
   (<>) = (+)

instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = []
  toString (Single a s)   = s 
  toString (Append m l r) = (toString l) ++ (toString r)

  fromString [] = Empty
  fromString s  = foldl (+++) Empty (map createItem $ lines s)
        where createItem x = Single (scoreString x, Size 1) x

  line         = indexJ

  replaceLine n l b = (takeJ n b) +++ (fromString l) +++ (dropJ (n + 1) b) 

  numLines Empty          = 0
  numLines (Single _ _)   = 1
  numLines (Append m _ _) = getIntSize m 

  value Empty                  = 0
  value (Single (a, _) _)      = getScore a
  value (Append (a, _) l r)    = getScore a

content :: JoinList (Score, Size) String
content = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String

main = runEditor editor content

