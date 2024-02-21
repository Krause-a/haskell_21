module Day3 (solve) where
import Data.List (groupBy, sortBy)
import Data.Function (on)


solve :: String -> String
solve s = solve_day_3_part_1 s ++ "\n" ++ solve_day_3_part_2 s

solve_day_3_part_1 :: String -> String
solve_day_3_part_1 xs = let intList = map listCharsToInt $ lines xs
                            commonBits = map (\x -> if x > 0 then 1 else 0) $ foldl1 (zipWith commonBitStep) intList
                            gamma = powerTwo commonBits
                            epsilon = powerTwo $ invertBits commonBits
                            in show (gamma * epsilon)



solve_day_3_part_2 :: String -> String
solve_day_3_part_2 xs = let intList = map listCharsToInt $ lines xs
                            oxygen = bitCriteria True intList
                            co2 = bitCriteria False intList
                            in show (powerTwo oxygen * powerTwo co2)


listCharsToInt :: String -> [Int]
listCharsToInt = map (\x -> read [x])



bitCriteria :: Bool -> [[Int]] -> [Int]
bitCriteria _ [x] = x
bitCriteria wc xs = head $ _bitCriteria 0 wc xs

_bitCriteria :: Int -> Bool -> [[Int]] -> [[Int]]
_bitCriteria _ _ [x] = [x]
_bitCriteria index wantCommon xs = let
    groups = groupBy (\x y -> ((x!!index) == (y!!index))) (sortBy (compare `on` (!!index)) xs)
    sortedGroups = sortBy (compare `on` (\x -> ((head x)!!index) == commonBit)) groups
    matches = --trace ("Want Common: " ++ show wantCommon ++ " Common: " ++ show commonBit ++ " Index: " ++ show index ++ " Sorted: " ++ show sortedGroups)
        sortedGroups !! if wantCommon then 1 else 0
    in _bitCriteria (index + 1) wantCommon matches
    where 
        columnSum = foldl (\acc x -> acc + if x > 0 then 1 else -1) 0 $ map (!!index) xs
        commonBit = if columnSum >= 0 then 1 else 0

powerTwo :: [Int] -> Int
powerTwo xs = sum $ zipWith (*) (reverse xs) (map (2^) [0,1..])

invertBits :: [Int] -> [Int]
invertBits = map (\x -> if x == 1 then 0 else 1)

commonBitStep :: Int -> Int -> Int
commonBitStep acc x = acc + step x
    where step y = if y == 1 then 1 else -1

