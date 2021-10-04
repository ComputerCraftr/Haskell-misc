module Karatsuba
    ( karatsubaMultiplyRecursive
    ) where

karatsubaMultiplyRecursive :: (Integral a, Show a) => a -> a -> a
karatsubaMultiplyRecursive multiplicand multiplier
    | multiplicand < 10 || multiplier < 10 = multiplicand * multiplier
    | otherwise = do
        let m2 = min (length (show multiplicand)) (length (show multiplier)) `div` 2

        let m2_digit_shift = 10^m2
        let m_digit_shift = m2_digit_shift * m2_digit_shift

        let high1 = multiplicand `div` m2_digit_shift
        let low1 = multiplicand `mod` m2_digit_shift
        let high2 = multiplier `div` m2_digit_shift
        let low2 = multiplier `mod` m2_digit_shift

        let z0 = karatsubaMultiplyRecursive low1 low2
        let z1 = karatsubaMultiplyRecursive (low1 + high1) (low2 + high2)
        let z2 = karatsubaMultiplyRecursive high1 high2

        (z2 * m_digit_shift) + ((z1 - z2 - z0) * m2_digit_shift) + z0
