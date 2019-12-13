{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

-- Ex 3

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where 
    mempty = Score 0

instance Semigroup Score where
   (<>) = (+)

score :: Char -> Score
score c | c `elem` "dg"    = Score 2
        | c `elem` "bcmp"  = Score 3
        | c `elem` "fhvwy" = Score 4
        | c   ==   'k'     = Score 5
        | c `elem` "jx"    = Score 8
        | c `elem` "qz"    = Score 10
        | otherwise        = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score