{-# LANGUAGE BangPatterns #-}
module Collatz (
    find3nPlus1
) where

import NumDigits ( integerDigitsBase10 )

find3nPlus1Helper :: Int -> Integer -> Integer -> (Int, Integer)
find3nPlus1Helper !steps !maxNum !n
    | n <= 2 = (steps + 1, integerDigitsBase10 maxNum)
    | even n = find3nPlus1Helper (steps + 1) (max n maxNum) (n `quot` 2)
    | otherwise = find3nPlus1Helper (steps + 1) (max n maxNum) (3 * n + 1)

find3nPlus1 :: Integer -> (Int, Integer)
find3nPlus1 = find3nPlus1Helper 0 0
