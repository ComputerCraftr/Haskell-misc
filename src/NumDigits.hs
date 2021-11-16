{-# LANGUAGE BangPatterns #-}
module NumDigits (
    integerDigitsBaseN,
    integerDigitsBase10,
    integerDigitsBase256
) where

integerDigitsBaseNHelper :: (Integral a) => a -> a -> a -> a
integerDigitsBaseNHelper !digits !base !number
    | number <= 0 = digits
    -- Divide by base until the number is rounded down to zero
    | otherwise = integerDigitsBaseNHelper (digits + 1) base (number `quot` base)

integerDigitsBaseN :: (Integral a) => a -> a -> a
integerDigitsBaseN = integerDigitsBaseNHelper 0

integerDigitsBase10 :: (Integral a) => a -> a
integerDigitsBase10 = integerDigitsBaseNHelper 0 10

integerDigitsBase256 :: (Integral a) => a -> a
integerDigitsBase256 = integerDigitsBaseNHelper 0 256
