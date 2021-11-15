module Karatsuba
    ( karatsubaMultiplyRecursive
    ) where

import NumDigits ( integerDigitsBase256 )

-- The Karatsuba multiplication algorithm can work with numbers in any base so long as they have more than one digit
karatsubaMultiplyRecursive :: (Integral a) => a -> a -> a
karatsubaMultiplyRecursive multiplicand multiplier
    | multiplicand < 256 || multiplier < 256 = multiplicand * multiplier
    | otherwise = do
        let m2 = min (integerDigitsBase256 multiplicand) (integerDigitsBase256 multiplier) `quot` 2

        let m2_digit_shift = 256 ^ m2
        let m_digit_shift = m2_digit_shift * m2_digit_shift

        let high1 = multiplicand `quot` m2_digit_shift
        let low1 = multiplicand `rem` m2_digit_shift
        let high2 = multiplier `quot` m2_digit_shift
        let low2 = multiplier `rem` m2_digit_shift

        let z0 = karatsubaMultiplyRecursive low1 low2
        let z1 = karatsubaMultiplyRecursive (low1 + high1) (low2 + high2)
        let z2 = karatsubaMultiplyRecursive high1 high2

        (z2 * m_digit_shift) + ((z1 - z2 - z0) * m2_digit_shift) + z0
