toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigits (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = (toDigits (n `div` 10)) ++ [n `mod` 10]

doubleItems :: [Integer] -> [Integer]
doubleItems [] = []
doubleItems (x:[]) = [x]
doubleItems (x:y:rest)
  | length(x:y:rest) `mod` 2 == 0 = (2*x):y:(doubleItems rest)
  | otherwise                     = x:(2*y):(doubleItems rest)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x <= 0    = sumDigits xs
  | x < 10    = x + sumDigits xs
  | otherwise = x `mod` 10 + sumDigits ((x `div` 10) : xs )

validate :: Integer -> Bool
validate n = ((sumDigits (doubleItems (toDigits n))) `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(start,end)]
hanoi n start end temp =
    let nMinusOne = subtract 1 n
    in hanoi nMinusOne start temp end ++
       hanoi 1 start end temp ++
       hanoi nMinusOne temp end start
