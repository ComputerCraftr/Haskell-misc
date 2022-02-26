{-# LANGUAGE BangPatterns #-}
module CompositePowers (
    find2PowNMinus1Factors,
    sumPowersFast
) where

-- Based on the proof that for composite n, 2 ^ n - 1 will also be composite, we can generate factors for 2 ^ n - 1 from factors of n
-- This could also be used recursively to speed up factoring a number z which can be written in this form as follows:
-- Step 1 - write z in the form 2 ^ n - 1
-- Step 2 - find two factors of n if it is composite, otherwise exit
-- Step 3 - run this function on the two factors of n to receive two factors of z
-- Step 4 - the first factor x can also be written in the form 2 ^ n - 1, so continue from step 1 with z = x
find2PowNMinus1Factors :: (Integral a) => a -> a -> (a, a)
find2PowNMinus1Factors factorNOne factorNTwo =
    let x = 2 ^ factorNTwo - 1
        y = sumPowersFast 2 factorNTwo (factorNOne - 1)
    in (x, y)

-- sum(base^(n*expMulti),n,0,terms) = (base^((terms + 1) * expMulti) - 1) / (base^expMulti - 1)
sumPowersFast :: (Integral a) => a -> a -> a -> a
sumPowersFast base expMulti terms =
    let expMultiPow = base ^ expMulti
    in (expMultiPow ^ (terms + 1) - 1) `quot` (expMultiPow - 1)

-- sum(base^(n*expMulti),n,0,terms)
sumPowersLoop :: (Integral a) => a -> a -> a -> a -> a
sumPowersLoop !base !expMulti !terms !num
    | terms <= 0 = num + 1
    | otherwise = sumPowersLoop base expMulti (terms - 1) (num + base ^ (terms * expMulti))
