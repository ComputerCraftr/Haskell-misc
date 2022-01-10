module Lib (
    someFunc
) where

import BFInterpreter ( runBF )
import TSP ( City(..), genRandomCities, tspSolverGreedy, tspSolverGreedyDeterministic )

someFunc :: IO ()
someFunc = do
    --putStrLn "Input brainfuck program:"
    --inputCode <- getLine
    runBF "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    testCities <- genRandomCities 30
    let testCitiesReversed = reverse testCities
    putStrLn "Greedy TSP Solver:"
    print (tspSolverGreedy testCities)
    print (tspSolverGreedy testCitiesReversed)
    putStrLn "Deterministic greedy TSP Solver:"
    print (tspSolverGreedyDeterministic testCities)
    print (tspSolverGreedyDeterministic testCitiesReversed)
    print (tspSolverGreedyDeterministic [City (5, 9), City (2, 7), City (8, 3), City (1, 6)])
