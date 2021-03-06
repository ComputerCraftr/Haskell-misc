{-# LANGUAGE BangPatterns #-}
module IOTest (
    getInt,
    getInteger,
    getSingleChar,
    getTickets,
    printTickets
) where

import Text.Read ( readMaybe )
import Data.Maybe ( isNothing, fromJust, maybe )

getInt :: IO Int
getInt = do
    putStrLn "Please enter an integer:"
    inputLine <- getLine
    let inputNum = readMaybe inputLine :: Maybe Int
    maybe getInt return inputNum

getInteger :: IO Integer
getInteger = do
    putStrLn "Please enter an integer:"
    inputLine <- getLine
    let inputNum = readMaybe inputLine :: Maybe Integer
    maybe getInteger return inputNum

getSingleChar :: IO Char
getSingleChar = do
    putStrLn "Please enter a character:"
    inputLine <- getLine
    if length inputLine == 1 then
        return $! head inputLine
    else
        getSingleChar

getTicketsSection :: Char -> Int -> IO Int
getTicketsSection !currentSection !numSeats = do
    putStrLn ("How many tickets were sold for section " ++ [currentSection] ++ "?")
    inputLine <- getLine
    let inputNum = readMaybe inputLine :: Maybe Int
    if isNothing inputNum || inputNum < Just 0 || inputNum > Just numSeats then do
        putStrLn ("There are only " ++ show numSeats ++ " seats in section " ++ [currentSection] ++ ". Please enter a valid number.")
        getTicketsSection currentSection numSeats
    else
        return $! fromJust inputNum

getTickets :: IO [Int]
getTickets = do
    ticketsA <- getTicketsSection 'A' 300
    ticketsB <- getTicketsSection 'B' 500
    ticketsC <- getTicketsSection 'C' 200
    return [ticketsA, ticketsB, ticketsC]

printTickets :: [Int] -> IO ()
printTickets ticketsList
    | length ticketsList == 3 = do
        putStrLn "Tickets sold for section:"
        putStrLn ("A: " ++ (show . head) ticketsList)
        putStrLn ("B: " ++ show (ticketsList !! 1))
        putStrLn ("C: " ++ (show . last) ticketsList)
    | otherwise = putStrLn "Invalid input"
