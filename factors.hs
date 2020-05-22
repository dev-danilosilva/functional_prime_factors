module Factors where
    factors :: Integer -> [Integer]
    factors n = [x | x <- [1..n], n `mod` x == 0]

    prime :: [Integer] -> [Integer]
    prime xs = filter (\x -> factors x == [1, x]) xs

    maxof :: [Integer] -> Integer
    maxof xs = foldr (\x xs -> if x > xs then x else xs) 0 xs

    mpf :: Integer -> Integer
    mpf = maxof . prime . factors
