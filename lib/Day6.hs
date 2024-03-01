module Day6 (solve) where
import Common (toIntList, splitAll)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 = solveFish 80

solve_day_part_2 :: String -> String
solve_day_part_2 = solveFish 256


type Fish = Int -- Spawn Time


solveFish :: Int -> String -> String
solveFish dr s = let
    fishies = parseInput s
    m = createMap dr fishies
    in show $ foldr (\f acc -> acc + m Map.! (dr-f)) 0 fishies + foldr (\_ acc -> acc + 1) 0 fishies


createMap :: Int -> [Fish] -> Map.Map Fish Int
createMap _ [] = Map.empty
createMap dr (f:fishies) = getFishDays dr f (createMap dr fishies)

getFishDays :: Int -> Fish -> Map.Map Fish Int -> Map.Map Fish Int
getFishDays dr f m
    | dr >= f = case Map.lookup spawnDay m of
                Just _ -> m
                Nothing -> let
                    mapA = getFishDays spawnDay 7 m
                    mapB = getFishDays spawnDay 9 mapA
                    recFish = fishValue 7 mapB
                    newFish = 1 + fishValue 9 mapB
                    upMap = Map.insert spawnDay (recFish + newFish) mapB
                    in upMap
    | otherwise = m
        where
            spawnDay = dr - f
            fishValue fv mv = fromMaybe 0 $ Map.lookup (spawnDay - fv) mv


parseInput :: String -> [Fish]
parseInput = map (+1) . toIntList . splitAll ','
