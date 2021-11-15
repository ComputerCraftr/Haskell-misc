{-# LANGUAGE BangPatterns #-}
module Collatz
    ( find3nPlus1
    ) where

import NumDigits ( integerDigitsBase10 )

find3nPlus1Helper :: Integer -> Int -> Integer -> (Int, Integer)
find3nPlus1Helper !n !steps !maxNum
    | n <= 2 = (steps + 1, integerDigitsBase10 maxNum)
    | even n = find3nPlus1Helper (n `quot` 2) (steps + 1) (max n maxNum)
    | otherwise = find3nPlus1Helper (3 * n + 1) (steps + 1) (max n maxNum)

find3nPlus1 :: Integer -> (Int, Integer)
find3nPlus1 n = find3nPlus1Helper n 0 0
