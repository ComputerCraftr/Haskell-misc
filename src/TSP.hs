{-# LANGUAGE BangPatterns #-}
module TSP (
    City (..),
    cityDist,
    tspSolverGreedy,
    genRandomCities
) where

import System.Random ( randomRIO )

newtype City = City (Int, Int) deriving (Read, Show, Eq)

getCityX :: City -> Int
getCityX (City a) = fst a

getCityY :: City -> Int
getCityY (City a) = snd a

cityDist :: City -> City -> Float
cityDist !cityOne !cityTwo = (sqrt . fromIntegral) ((getCityX cityTwo - getCityX cityOne) ^ 2 + (getCityY cityTwo - getCityY cityOne) ^ 2)

genRandomCities :: Int -> IO [City]
genRandomCities = genRandomCitiesHelper [] 0

genRandomCitiesHelper :: [City] -> Int -> Int -> IO [City]
genRandomCitiesHelper !cities !count !numCities
    | count == numCities = return cities
    | otherwise = do
        x <- randomRIO (0, 100) :: IO Int
        y <- randomRIO (0, 100) :: IO Int
        genRandomCitiesHelper (City (x, y):cities) (count + 1) numCities

tspSolverGreedy :: [City] -> (Float, [Int])
tspSolverGreedy !cities = tspSolverGreedyHelper cities (length cities) [0] 0 0 (-1) 0

tspSolverGreedyHelper :: [City] -> Int -> [Int] -> Float -> Int -> Float -> Int -> (Float, [Int])
tspSolverGreedyHelper !cities !citiesLen !visitedIdx !totalDist !checkingIdx !minDist !lowestDistIdx
    | checkingIdx == citiesLen = tspSolverGreedyHelper cities citiesLen (lowestDistIdx:visitedIdx) (totalDist + minDist) 0 (-1) 0
    | length visitedIdx < citiesLen =
        let tspSolverGreedyNextIdx = tspSolverGreedyHelper cities citiesLen visitedIdx totalDist (checkingIdx + 1)
            tspSolverGreedyNoUpdate = tspSolverGreedyNextIdx minDist lowestDistIdx
        in if checkingIdx `elem` visitedIdx then
            tspSolverGreedyNoUpdate
        else
            let dist = cityDist (cities !! head visitedIdx) (cities !! checkingIdx) in
            if dist < minDist || minDist == -1 then
                tspSolverGreedyNextIdx dist checkingIdx
            else
                tspSolverGreedyNoUpdate
    | otherwise = (totalDist, reverse visitedIdx)
