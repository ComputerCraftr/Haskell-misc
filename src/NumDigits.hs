{-# LANGUAGE BangPatterns #-}
module NumDigits 
    ( integerDigitsBaseN
    , integerDigitsBase10
    , integerDigitsBase256
    ) where

integerDigitsBaseNHelper :: (Integral a) => a -> a -> a -> a
integerDigitsBaseNHelper !number !base !digits
    | number <= 0 = digits
    -- Divide by base until the number is rounded down to zero
    | otherwise = integerDigitsBaseNHelper (number `quot` base) base (digits + 1)

integerDigitsBaseN :: (Integral a) => a -> a -> a
integerDigitsBaseN number base = integerDigitsBaseNHelper number base 0

integerDigitsBase10 :: (Integral a) => a -> a
integerDigitsBase10 number = integerDigitsBaseNHelper number 10 0

integerDigitsBase256 :: (Integral a) => a -> a
integerDigitsBase256 number = integerDigitsBaseNHelper number 256 0
