module Day7 (solve) where
import Common (toIntList, splitAll)
import Data.List (maximumBy, minimumBy, group, sort)
import Data.Function (on)
import Debug.Trace (trace)

solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 s = let
    nums = toIntList $ splitAll ',' s
    med = median nums
    sumDiff = foldr (\x acc -> acc + abs (med - x)) 0 nums
    in show sumDiff

solve_day_part_2 :: String -> String
solve_day_part_2 s = let
    nums = toIntList $ splitAll ',' s
    m = maximum nums
    pairs = pairResultsToIndex m nums
    in show $  minimumBy (compare `on` snd) pairs


mode :: [Int] -> Int
mode = (!!0) . maximumBy (compare `on` length) . group . sort

median :: [Int] -> Int
median xs = sort xs !! mid
    where
        len = length xs - 1
        mid = len `div` 2

nSum :: Int -> Int
nSum x = (x * (x + 1)) `div` 2

pairResultsToIndex :: Int -> [Int] -> [(Int, Int)]
pairResultsToIndex index list
    | index == 1 = []
    | otherwise = (index, getCost list) : pairResultsToIndex (pred index) list
    where
        getCost = foldr (\x acc -> acc + nSum (abs (index - x))) 0
