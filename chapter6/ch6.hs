-- chapter 6: higher order functions

-- 6.1 curried functions

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- 6.2 some higher-orderism is in order

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- can be read as the function zipWith' inputs (f outputs c) -> outputs (f' outputs [c])
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]          -- first parameter is a function (f) that takes two things and produces a third thing (such as max, min, (++), (*))
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys     -- examples: (++) [1,2,3] [4,5,6], (max) [7,2,9] [1, 5, 10]

flip' :: (a -> b -> c) -> (b -> a -> c)                 -- f -> f'
flip' f y x = f x y

-- 6.3 maps and filters

chain :: (Integral a) => a -> [a]                       -- Collatz sequence
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- 6.4 lambdas

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- 6.5 only folds and horses

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs                -- \acc is a binary function, 0 is the starting value and xs is the list to be folded up

-- 6.6 function application with $

-- 6.7 function composition

