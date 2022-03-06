{-# LANGUAGE BangPatterns #-}
module TSP (
    City (..),
    cityDist,
    trilaterateCity,
    genRandomCities,
    tspSolverGreedy,
    tspSolverGreedyDeterministic
) where

import System.Random ( randomRIO )

newtype City = City (Int, Int) deriving (Read, Show, Eq)

getCityX :: City -> Int
getCityX (City a) = fst a

getCityY :: City -> Int
getCityY (City a) = snd a

-- The distance between two points is calculated using the Pythagorean theorem
cityDist :: City -> City -> Double
cityDist !cityOne !cityTwo = (sqrt . fromIntegral) ((getCityX cityTwo - getCityX cityOne) ^ 2 + (getCityY cityTwo - getCityY cityOne) ^ 2)

-- The coordinates for a point can be calculated using the distance to two or three known locations (for example, (0, 0) and (100, 0) here) by treating the distances as radii,
-- setting the circle equations equal to each other, solving for x, and finally plugging the result back into one of the circle equations for y. Normally, this would produce two
-- solutions from the positive and negative square roots, but we have restricted the domain to the first quadrant here, so we know that the positive root is the correct solution.
trilaterateCity :: Double -> Double -> City
trilaterateCity !originDist !secondDist =
    -- Solving sqrt(r_1^2 - x^2) = sqrt(r_2^2 - (x - h)^2) for x results in x = (r_1^2 - r_2^2 + h^2) / 2h (equation for trilateration)
    let x_coord = (originDist ^ 2 - secondDist ^ 2 + 10000) / 200
        y_coord = sqrt (originDist ^ 2 - x_coord ^ 2)
    in City (round x_coord, round y_coord)

genRandomCities :: Int -> IO [City]
genRandomCities = genRandomCitiesHelper [] 0

genRandomCitiesHelper :: [City] -> Int -> Int -> IO [City]
genRandomCitiesHelper !cities !count !numCities
    | count == numCities = return cities
    | otherwise = do
        x <- randomRIO (0, 100) :: IO Int
        y <- randomRIO (0, 100) :: IO Int
        genRandomCitiesHelper (City (x, y):cities) (count + 1) numCities

tspSolverGreedy :: [City] -> (Double, [Int])
tspSolverGreedy !cities = tspSolverGreedyHelper cities (length cities) [0] 1 0 0 (fromIntegral (maxBound :: Int)) 0

tspSolverGreedyHelper :: [City] -> Int -> [Int] -> Int -> Double -> Int -> Double -> Int -> (Double, [Int])
tspSolverGreedyHelper !cities !citiesLen !visitedIdx !visitedLen !totalDist !checkingIdx !minDist !lowestDistIdx
    | checkingIdx == citiesLen = tspSolverGreedyHelper cities citiesLen (lowestDistIdx:visitedIdx) (visitedLen + 1) (totalDist + minDist) 0 (fromIntegral (maxBound :: Int)) 0
    | visitedLen < citiesLen =
        let tspSolverGreedyNextIdx = tspSolverGreedyHelper cities citiesLen visitedIdx visitedLen totalDist (checkingIdx + 1)
            tspSolverGreedyNoUpdate = tspSolverGreedyNextIdx minDist lowestDistIdx
            currentCity = cities !! checkingIdx
            dist = cityDist (cities !! head visitedIdx) currentCity
            distEqual = dist == minDist
            currentY = getCityY currentCity
            currentX = getCityX currentCity
            lowestY = getCityY (cities !! lowestDistIdx)
            lowestX = getCityX (cities !! lowestDistIdx)
        -- In case distance is equal, use y coordinate and then x coordinate as tie breakers
        in if checkingIdx `elem` visitedIdx || dist > minDist || (distEqual && currentY > lowestY) || (distEqual && currentY == lowestY && currentX > lowestX) then
            tspSolverGreedyNoUpdate
        else
            tspSolverGreedyNextIdx dist checkingIdx
    -- Add return trip
    | otherwise = (totalDist + cityDist (cities !! head visitedIdx) (head cities), reverse visitedIdx)

firstClosestToOrigin :: [City] -> [City]
firstClosestToOrigin !cities = firstClosestToOriginHelper cities (length cities) 0 (fromIntegral (maxBound :: Int)) 0

firstClosestToOriginHelper :: [City] -> Int -> Int -> Double -> Int -> [City]
firstClosestToOriginHelper !cities !citiesLen !checkingIdx !minDist !lowestDistIdx
    | checkingIdx == citiesLen =
        case lowestDistIdx of
            0 -> cities
            _ -> let (firstHalf, secondHalf) = splitAt lowestDistIdx cities
                -- Swap the first city with the one closest to (0, 0)
                in cities !! lowestDistIdx:tail firstHalf ++ head firstHalf:tail secondHalf
    | otherwise =
        let originCity = City (0, 0)
            currentCity = cities !! checkingIdx
            dist = cityDist originCity currentCity
            distEqual = dist == minDist
            currentY = getCityY currentCity
            currentX = getCityX currentCity
            lowestY = getCityY (cities !! lowestDistIdx)
            lowestX = getCityX (cities !! lowestDistIdx)
            firstClosestToOriginNextIdx = firstClosestToOriginHelper cities citiesLen (checkingIdx + 1)
            firstClosestToOriginNoUpdate = firstClosestToOriginNextIdx minDist lowestDistIdx
        -- In case distance is equal, use y coordinate and then x coordinate as tie breakers
        in if dist > minDist || (distEqual && currentY > lowestY) || (distEqual && currentY == lowestY && currentX > lowestX) then
            firstClosestToOriginNoUpdate
        else
            firstClosestToOriginNextIdx dist checkingIdx

-- This deterministic solver will always visit the cities from the list in the same order, regardless of how the list of cities itself is sorted
tspSolverGreedyDeterministic :: [City] -> (Double, [Int])
tspSolverGreedyDeterministic = tspSolverGreedy . firstClosestToOrigin
