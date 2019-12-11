import Data.Bits

-- Ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Ex 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : [fibs2!!(x - 1) + fibs2!!(x - 2) | x <- [2..]]

-- Ex 3
data Stream a =  Stream a (Stream a) 

streamToList :: Stream a -> [a]
streamToList (Stream x next) = x : (streamToList next)

instance Show a => Show (Stream a) where
     show = show . ((take 20) . streamToList)

-- Ex 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s (streamFromSeed f (f s))

-- Ex 5
nats :: Stream Integer
nats = streamFromSeed (1+) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream val1 next1) s2 = Stream val1 (interleaveStreams s2 next1)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (1+) ruler)