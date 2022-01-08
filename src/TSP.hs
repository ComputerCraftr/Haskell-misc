{-# LANGUAGE BangPatterns #-}
module TSP (
    City (..),
    cityDist,
    genRandomCities,
    trilaterateCity,
    tspSolverGreedy
) where

import System.Random ( randomRIO )

newtype City = City (Int, Int) deriving (Read, Show, Eq)

getCityX :: City -> Int
getCityX (City a) = fst a

getCityY :: City -> Int
getCityY (City a) = snd a

-- The distance between two points is calculated using the Pythagorean theorem
cityDist :: City -> City -> Float
cityDist !cityOne !cityTwo = (sqrt . fromIntegral) ((getCityX cityTwo - getCityX cityOne) ^ 2 + (getCityY cityTwo - getCityY cityOne) ^ 2)

genRandomCities :: Int -> IO [City]
genRandomCities = genRandomCitiesHelper [] 0

-- The coordinates for a point can be calculated using the distance to two or three known locations (for example, (0, 0) and (100, 0) here) by treating the distances as radii,
-- setting the circle equations equal to each other, solving for x, and finally plugging the result back into one of the circle equations for y. Normally, this would produce two
-- solutions from the positive and negative square roots, but we have restricted the domain to the first quadrant here, so we know that the positive root is the correct solution.
trilaterateCity :: Float -> Float -> City
trilaterateCity !originDist !secondDist =
    -- Solving sqrt(r_1^2 - x^2) = sqrt(r_2^2 - (x - h)^2) for x results in x = (r_1^2 - r_2^2 + h^2) / 2h (equation for trilateration)
    let x_coord = (originDist ^ 2 - secondDist ^ 2 + 10000) / 200
        y_coord = sqrt (originDist ^ 2 - x_coord ^ 2)
    in City (round x_coord, round y_coord)

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
