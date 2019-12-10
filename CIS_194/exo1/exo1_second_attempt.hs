-- Ex 1

toDigits :: Integer -> [Integer]
toDigits n 
  | n <= 0    = []
  | otherwise = (toDigits (n `div` 10)) ++ [(n `mod` 10)] 


revList :: [Integer] -> [Integer]
revList [] = []
revList (x:[]) = [x]
revList (x:xs) = (revList xs) ++ [x]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = revList (toDigits n)

-- Ex 2
doubleAlt :: [Integer] -> [Integer]
doubleAlt []         = []
doubleAlt (x:[])     = [x]
doubleAlt (x:(y:xs)) = x : 2 * y : doubleAlt xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = (revList (doubleAlt (revList l)))

-- Ex 3
sumDigitsForInt :: Integer -> Integer
sumDigitsForInt 0 = 0
sumDigitsForInt n = (n `mod` 10) + sumDigitsForInt (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:[]) = sumDigitsForInt x
sumDigits (x:xs) = (sumDigitsForInt x) + (sumDigits xs)

-- Ex 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits (n)))) `mod` 10 == 0


-- Ex 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)