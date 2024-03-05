module Day7 (solve) where
import Common (toIntList, splitAll)
import Data.List (maximumBy, group, sort)
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
solve_day_part_2 = const "Part Two"


mode :: [Int] -> Int
mode = (!!0) . maximumBy (compare `on` length) . group . sort

median :: [Int] -> Int
median xs = sort xs !! mid
    where
        len = length xs - 1
        mid = len `div` 2
