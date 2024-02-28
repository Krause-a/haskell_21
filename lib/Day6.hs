module Day6 (solve) where
import Common (toIntList, splitAll)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 s = let 
    fishies = parseInput s
    m = trace (show (fishMap fishies)) fishMap fishies
    balanceFishies = map balanceSpawnTime fishies
    fishCount = map (\x -> fromMaybe 0 (x `Map.lookup` m)) $ trace ("Balanced Fish: " ++ show balanceFishies) balanceFishies
    in show (sum fishCount)

solve_day_part_2 :: String -> String
solve_day_part_2 _ = "Two"

daysRemaining :: Int
daysRemaining = 80

parseInput :: String -> [Fish]
parseInput = toIntList . splitAll ','

fishMap :: [Fish] -> Map.Map Int Int
fishMap fishies = let
    drs = Map.fromSet id . Set.fromList $ map balanceSpawnTime fishies
    in foldr (\x acc -> Map.insert (trace ("Fish: " ++ show x ++ ", Count: " ++ show (newFish x)) x) (newFish x) acc) Map.empty drs


type Fish = Int -- Spawn Time

balanceSpawnTime :: Fish -> Int
balanceSpawnTime st = 8 - st + daysRemaining

newFish :: Fish -> Int
newFish dr
    | dr < 9 = 1
    | otherwise = 1 + newFish (dr - 9) + oldFish (dr - 9)

oldFish :: Fish -> Int
oldFish dr
    | dr < 7 = 0
    | otherwise = newFish (dr - 7) + oldFish (dr - 7)
