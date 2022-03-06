module Lib (
    someFunc
) where

import BFInterpreter (runBF)
import TSP (City(..), genRandomCities, tspSolverGreedy, tspSolverGreedyDeterministic)
import CompositePowers (find2PowNMinus1Factors, findPowNMinus1Factors)

printList :: (a -> IO ()) -> [a] -> IO ()
printList f (x:xs) = do
    f x
    printList f xs
printList f [] = putChar '\n'

someFunc :: IO ()
someFunc = do
    {-
    putStrLn "Input brainfuck program:"
    inputCode <- getLine
    runBF "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    -}

    testCities <- genRandomCities 30
    let testCitiesReversed = reverse testCities
    putStrLn "Greedy TSP Solver:"
    print (tspSolverGreedy testCities)
    print (tspSolverGreedy testCitiesReversed)
    putStrLn "Deterministic greedy TSP Solver:"
    print (tspSolverGreedyDeterministic testCities)
    print (tspSolverGreedyDeterministic testCitiesReversed)

    {-
    let printFst idx = print (testCities !! idx)
    let printSnd idx = print (testCitiesReversed !! idx)
    printList printFst ((snd . tspSolverGreedyDeterministic) testCities)
    printList printSnd ((snd . tspSolverGreedyDeterministic) testCitiesReversed)
    -}
