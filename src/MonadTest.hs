module MonadTest
    ( lastThreeElementsLessThan100
    ) where

-- if the last element is less than 100, pop it off the list otherwise return nothing
lessThan100 :: [Int] -> Maybe [Int]
lessThan100 a
    | last a < 100 = (Just . init) a
    | otherwise = Nothing

-- monad which pops the last three elements off of a list if they are less than 100 otherwise returns nothing
-- this is useful for applying several functions to an input which depend on the outputs of each other but may fail at each step
lastThreeElementsLessThan100 :: [Int] -> Maybe [Int]
lastThreeElementsLessThan100 arr = lessThan100 arr >>= lessThan100 >>= lessThan100
