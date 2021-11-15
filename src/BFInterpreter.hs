{-# LANGUAGE BangPatterns #-}
module BFInterpreter (
    runBF
) where

import Data.Char ( chr, ord )
import IOTest ( getSingleChar )

runBF :: [Char] -> IO ()
runBF code = do
    runBFHelper code (length code) 0 (repeat 0) 0 []
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

-- This function is designed to be called on a string with the index immediately after the opening
-- bracket, skip through any nested brackets, and return the index after the closing bracket
skipClosingBrackets :: [Char] -> Int -> Int -> Int
skipClosingBrackets [] !index _ = index
skipClosingBrackets !charList !index !openBrackets = case charList !! index of
    ']' -> do
        if openBrackets > 1 then
            skipClosingBrackets charList (index + 1) (openBrackets - 1)
        else
            index + 1
    '[' -> skipClosingBrackets charList (index + 1) (openBrackets + 1)
    _   -> skipClosingBrackets charList (index + 1) openBrackets

runBFHelper :: [Char] -> Int -> Int -> [Int] -> Int -> [Int] -> IO ()
runBFHelper !code !codeLen !index !stack !pointer !loopStartIndexes
    | null code || index == codeLen = return ()
    | otherwise = case code !! index of
        '>' -> runBFHelper code codeLen (index + 1) stack (pointer + 1) loopStartIndexes
        '<' -> runBFHelper code codeLen (index + 1) stack (max (pointer - 1) 0) loopStartIndexes
        '+' -> runBFHelper code codeLen (index + 1) (incrementElement stack pointer) pointer loopStartIndexes
        '-' -> runBFHelper code codeLen (index + 1) (decrementElement stack pointer) pointer loopStartIndexes
        '.' -> do
            putStr [chr (stack !! pointer)]
            runBFHelper code codeLen (index + 1) stack pointer loopStartIndexes
        ',' -> do
            inputChar <- getSingleChar
            runBFHelper code codeLen (index + 1) (setElement stack pointer (ord inputChar)) pointer loopStartIndexes
        '[' -> do
            if stack !! pointer > 0 then
                let loopStartIndex = index + 1 in
                runBFHelper code codeLen loopStartIndex stack pointer (loopStartIndex:loopStartIndexes)
            else
                runBFHelper code codeLen (skipClosingBrackets code (index + 1) 1) stack pointer loopStartIndexes
        ']' -> do
            if stack !! pointer > 0 then
                runBFHelper code codeLen (head loopStartIndexes) stack pointer loopStartIndexes
            else
                runBFHelper code codeLen (index + 1) stack pointer (tail loopStartIndexes)
        _   -> putStrLn ("Invalid operator \"" ++ [code !! index] ++ "\" encountered.")
