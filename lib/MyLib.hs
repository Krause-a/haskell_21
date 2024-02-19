module MyLib (daySelector) where
import Data.List (intercalate)
import Text.Read (readMaybe)
import Debug.Trace

daySelector :: Int -> String -> String
daySelector d | d == 1 = solve_day_1
              | d == 2 = solve_day_2
              | d == 3 = solve_day_3
              | otherwise = id

solve_day_3 :: String -> String
solve_day_3 s = solve_day_3_part_1 s ++ "\n" ++ solve_day_3_part_2 s

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
                            in show $ oxygen ++ 9 : co2

solve_day_2 :: String -> String
solve_day_2 s = solve_day_2_part_1 s ++ "\n" ++ solve_day_2_part_2 s

solve_day_2_part_1 :: String -> String
solve_day_2_part_1 = show . uncurry (*) . foldl submarineFlatRoute (0, 0) . map words . lines

solve_day_2_part_2 :: String -> String
solve_day_2_part_2 = show . (\(_, x, y) -> x * y) . foldl submarineAimRoute (0, 0, 0) . map words . lines


solve_day_1 :: String -> String
solve_day_1 s = solve_day_1_part_1 s ++ "\n" ++ solve_day_1_part_2 s

solve_day_1_part_1 :: String -> String
solve_day_1_part_1  = show . sum . map compareDayOne . windows 2 . toIntList . splitAll '\n'

solve_day_1_part_2 :: String -> String
solve_day_1_part_2  = show . sum . map compareDayOne . windows 2 . map sum . windows 3 . toIntList . splitAll '\n'


bitCriteria :: Bool -> [[Int]] -> [Int]
bitCriteria _ [x] = x
bitCriteria wantCommon xs = let
    filtered = filter filter' (trace (show xs) xs)
    reducedColumns = map tail filtered
    -- The common bit is a lie. It works when there is a most common bit. When they are tied, I cannot assume the actual bit is the same as the tie breaker bit... Don't forget about trace. It is great!
    in commonBit : bitCriteria wantCommon reducedColumns
    where 
        columnSum = foldl (\acc x -> acc + if x > 0 then 1 else -1) 0 $ map head xs
        commonBit = case compare columnSum 0 of 
            LT -> 0
            EQ -> if wantCommon then 1 else 0
            GT -> 1
        filterCommon = (\x -> head x == commonBit)
        filterUncommon = (\x -> head x /= commonBit)
        filter' = if wantCommon then filterCommon else filterUncommon

invertBits :: [Int] -> [Int]
invertBits = map (\x -> if x == 1 then 0 else 1)

powerTwo :: [Int] -> Int
powerTwo xs = sum $ zipWith (*) (reverse xs) (map (2^) [0,1..])

toBinary :: [Int] -> String
toBinary xs = foldl (\acc x -> acc ++ if x > 0 then "1" else "0") "" xs

commonBitStep :: Int -> Int -> Int
commonBitStep acc x = acc + step x
    where step = \x -> if x == 1 then 1 else -1

listCharsToInt :: String -> [Int]
listCharsToInt [] = []
listCharsToInt (x:xs) = read [x] : listCharsToInt xs

submarineAimRoute :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
submarineAimRoute (aim, x, y) [dir, disStr]
    | dir == "forward" = (aim, dis + x, dis * aim + y)
    | dir == "down" = (aim + dis, x, y)
    | dir == "up" = (aim - dis, x, y)
    where dis = read disStr

submarineFlatRoute :: (Int, Int) -> [String] -> (Int, Int)
submarineFlatRoute (x, y) [dir, dis] | dir == "forward" = (x + read dis, y)
                                 | dir == "down" = (x, y + (read dis)) 
                                 | dir == "up" = (x, y - (read dis)) 

merge :: (String, String) -> String
merge = uncurry (++)

toIntList :: [String] -> [Int]
toIntList [] = []
toIntList (x:xs) = case readMaybe x of
                Just a -> a : toIntList xs
                Nothing -> toIntList xs

compareDayOne :: [Int] -> Int
compareDayOne [x, y] | x < y = 1
               | otherwise = 0

windows :: Int -> [a] -> [[a]]
windows size [] = []
windows size xs
            | length xs >= size = take size xs : windows size (drop 1 xs)
            | otherwise = []

split :: Char -> String -> (String, String)
split sep [] = ("", "")
split sep (x:xs) 
                | x == sep = ("", xs)
                | x /= sep = let (first, second) = split sep xs in (x : first, second)

splitAll :: Char -> String -> [String]
splitAll _ [] = []
splitAll sep xs = let (first, second) = split sep xs in first : splitAll sep second

join :: String -> [String] -> String
join = intercalate
