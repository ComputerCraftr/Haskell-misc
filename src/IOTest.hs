module IOTest
    ( getTicketsA
    ) where

import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isNothing )

getTicketsA :: IO ()
getTicketsA = do
    putStrLn "How many tickets were sold for section A?"
    inputLine <- getLine
    let inputNum = readMaybe inputLine :: Maybe Int
    if isNothing inputNum || inputNum < Just 0 || inputNum > Just 300
        then do
            putStrLn "There are only 300 seats in section A. Please enter a valid number."
            getTicketsA
    else putStrLn ("There were " ++ (show . fromJust) inputNum ++ " tickets sold for section A.")
