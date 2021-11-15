{-# LANGUAGE BangPatterns #-}
module BFInterpreter (
    runBF
) where

import Data.Char ( chr, ord )
import IOTest ( getSingleChar )

runBF :: [Char] -> IO ()
runBF code = do
    runBFHelper code (repeat 0) 0
    putStrLn ""

incrementElement :: [Int] -> Int -> [Int]
incrementElement listInput element
    | null listInput || element < 0 = listInput
    | otherwise =
        let (firstHalf, secondHalf) = splitAt element listInput
            x:xs = secondHalf
        in case x of
            255 -> firstHalf ++ 0:xs
            _   -> firstHalf ++ (x+1):xs

decrementElement :: [Int] -> Int -> [Int]
decrementElement listInput element
    | null listInput || element < 0 = listInput
    | otherwise =
        let (firstHalf, secondHalf) = splitAt element listInput
            x:xs = secondHalf
        in case x of
            0 -> firstHalf ++ 255:xs
            _ -> firstHalf ++ (x-1):xs

setElement :: [Int] -> Int -> Int -> [Int]
setElement listInput element value
    | null listInput || element < 0 || value < 0 || value > 255 = listInput
    | otherwise =
        let (firstHalf, secondHalf) = splitAt element listInput
        in firstHalf ++ value:tail secondHalf

-- This function is designed to be called on a string after the opening bracket has been removed
-- and return everything inside, including nested brackets, up until the closing bracket is reached
stringInsideBrackets :: [Char] -> Int -> Int -> [Char]
stringInsideBrackets [] _ _ = []
stringInsideBrackets !listInput !openBrackets !index =
    let x = listInput !! index in case x of
        ']' -> do
            if openBrackets > 1 then
                stringInsideBrackets listInput (openBrackets - 1) (index + 1)
            else
                take index listInput
        '[' -> stringInsideBrackets listInput (openBrackets + 1) (index + 1)
        _   -> stringInsideBrackets listInput openBrackets (index + 1)

-- This function is designed to be called on a string after the opening bracket has been removed
-- and return everything after the closing bracket
skipClosingBrackets :: [Char] -> Int -> [Char]
skipClosingBrackets [] _ = []
skipClosingBrackets (x:xs) !openBrackets = case x of
    ']' -> do
        if openBrackets > 1 then
            skipClosingBrackets xs (openBrackets - 1)
        else
            xs
    '[' -> skipClosingBrackets xs (openBrackets + 1)
    _   -> skipClosingBrackets xs openBrackets

runBFLoop :: [Char] -> [Int] -> Int -> IO ([Int], Int)
runBFLoop !code !stack !pointer
    | null code || stack !! pointer == 0 = return (stack, pointer)
    | otherwise = do
        (newStack, newPointer) <- runBFHelper code stack pointer
        runBFLoop code newStack newPointer

runBFHelper :: [Char] -> [Int] -> Int -> IO ([Int], Int)
runBFHelper !code !stack !pointer
    | null code = return (stack, pointer)
    | otherwise = let x:xs = code in case x of
        '>' -> runBFHelper xs stack (pointer + 1)
        '<' -> runBFHelper xs stack (max (pointer - 1) 0)
        '+' -> runBFHelper xs (incrementElement stack pointer) pointer
        '-' -> runBFHelper xs (decrementElement stack pointer) pointer
        '.' -> do
            putStr [chr (stack !! pointer)]
            runBFHelper xs stack pointer
        ',' -> do
            inputChar <- getSingleChar
            runBFHelper xs (setElement stack pointer (ord inputChar)) pointer
        '[' -> do
            (newStack, newPointer) <- runBFLoop (stringInsideBrackets xs 1 0) stack pointer
            runBFHelper (skipClosingBrackets xs 1) newStack newPointer
        _   -> do
            putStrLn ("Invalid operator \"" ++ [x] ++ "\" encountered.")
            return ([], 0)
