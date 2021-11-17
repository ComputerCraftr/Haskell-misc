{-# LANGUAGE BangPatterns #-}
module BFInterpreter (
    runBF
) where

import Data.Int ( Int8 )
import Data.Char ( chr, ord )
import IOTest ( getSingleChar )

runBF :: [Char] -> IO ()
runBF code = do
    runBFHelper code (length code) [] 0 (repeat (0 :: Int8)) 0
    putChar '\n'

incrementElement :: [Int8] -> Int -> [Int8]
incrementElement listInput element
    | null listInput || element < 0 = listInput
    | otherwise =
        let (firstHalf, secondHalf) = splitAt element listInput
            x:xs = secondHalf
        in firstHalf ++ (x+1):xs

decrementElement :: [Int8] -> Int -> [Int8]
decrementElement listInput element
    | null listInput || element < 0 = listInput
    | otherwise =
        let (firstHalf, secondHalf) = splitAt element listInput
            x:xs = secondHalf
        in firstHalf ++ (x-1):xs

setElement :: [Int8] -> Int -> Int8 -> [Int8]
setElement listInput element value
    | null listInput || element < 0 || value < 0 = listInput
    | otherwise =
        let (firstHalf, secondHalf) = splitAt element listInput
        in firstHalf ++ value:tail secondHalf

-- This function is designed to be called on a string with the index immediately after the opening
-- bracket, skip through any nested brackets, and return the index after the closing bracket
skipClosingBrackets :: [Char] -> Int -> Int -> Int -> Int
skipClosingBrackets !charList !strLen !index !openBrackets
    | null charList || index == strLen = index
    | otherwise =
        let nextIndex = index + 1
            skipClosingBracketsNextIndex = skipClosingBrackets charList strLen nextIndex
        in case charList !! index of
            ']' -> do
                if openBrackets > 1 then
                    skipClosingBracketsNextIndex (openBrackets - 1)
                else
                    nextIndex
            '[' -> skipClosingBracketsNextIndex (openBrackets + 1)
            _   -> skipClosingBracketsNextIndex openBrackets

-- Fully tail recursive BF interpreter function
runBFHelper :: [Char] -> Int -> [Int] -> Int -> [Int8] -> Int -> IO ()
runBFHelper !code !codeLen !loopStartIndexes !index !stack !pointer
    | null code || index == codeLen = return ()
    | otherwise =
        let runBFCode = runBFHelper code codeLen
            nextIndex = index + 1
            runBFCodeNextIndex = runBFCode loopStartIndexes nextIndex
        in case code !! index of
            '>' -> runBFCodeNextIndex stack (pointer + 1)
            '<' -> do
                if pointer > 0 then
                    runBFCodeNextIndex stack (pointer - 1)
                else
                    putStrLn ("Attempted to use negative pointer at index " ++ show index ++ ".")
            '+' -> runBFCodeNextIndex (incrementElement stack pointer) pointer
            '-' -> runBFCodeNextIndex (decrementElement stack pointer) pointer
            '.' -> do
                (putChar . chr . fromIntegral . max 0) (stack !! pointer)
                runBFCodeNextIndex stack pointer
            ',' -> do
                inputChar <- getSingleChar
                runBFCodeNextIndex (setElement stack pointer ((fromIntegral . ord) inputChar)) pointer
            '[' -> do
                if stack !! pointer > 0 then
                    runBFCode (nextIndex:loopStartIndexes) nextIndex stack pointer
                else
                    runBFCode loopStartIndexes (skipClosingBrackets code codeLen nextIndex 1) stack pointer
            ']' -> do
                if null loopStartIndexes then
                    putStrLn ("Closing bracket with no previous opening bracket encountered at index " ++ show index ++ ".")
                else
                    if stack !! pointer > 0 then
                        runBFCode loopStartIndexes (head loopStartIndexes) stack pointer
                    else
                        runBFCode (tail loopStartIndexes) nextIndex stack pointer
            _   -> putStrLn ("Invalid operator \"" ++ [code !! index] ++ "\" encountered at index " ++ show index ++ ".")
