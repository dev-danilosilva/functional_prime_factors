module Factors where
    -- given an integer number, returns a list with its factors
    factors :: Integer -> [Integer]
    factors n = [x | x <- [1..n], n `mod` x == 0]

    -- given a list xs of integer, returns a list w/ the xs prime numbers
    prime :: [Integer] -> [Integer]
    prime xs = filter (\x -> factors x == [1, x]) xs

    -- given a list xs of itegers, returns the max value of xs
    maxof :: [Integer] -> Integer
    maxof xs = foldr (\x xs -> if x > xs then x else xs) 0 xs

    -- given an integer, returns the max of its prime factors
    mpf :: Integer -> Integer
    mpf = maxof . prime . factors
