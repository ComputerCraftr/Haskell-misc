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
tspSolverGreedy !cities = tspSolverGreedyHelper cities (length cities) [0] 1 0 0 (fromIntegral (maxBound :: Int)) 0

tspSolverGreedyHelper :: [City] -> Int -> [Int] -> Int -> Float -> Int -> Float -> Int -> (Float, [Int])
tspSolverGreedyHelper !cities !citiesLen !visitedIdx !visitedLen !totalDist !checkingIdx !minDist !lowestDistIdx
    | checkingIdx == citiesLen = tspSolverGreedyHelper cities citiesLen (lowestDistIdx:visitedIdx) (visitedLen + 1) (totalDist + minDist) 0 (fromIntegral (maxBound :: Int)) 0
    | visitedLen < citiesLen =
        let tspSolverGreedyNextIdx = tspSolverGreedyHelper cities citiesLen visitedIdx visitedLen totalDist (checkingIdx + 1)
            tspSolverGreedyNoUpdate = tspSolverGreedyNextIdx minDist lowestDistIdx
            dist = cityDist (cities !! head visitedIdx) (cities !! checkingIdx)
        in if checkingIdx `elem` visitedIdx || dist >= minDist then
            tspSolverGreedyNoUpdate
        else
            tspSolverGreedyNextIdx dist checkingIdx
    | otherwise = (totalDist, reverse visitedIdx)
