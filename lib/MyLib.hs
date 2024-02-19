module MyLib (daySelector) where
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Prelude hiding (compare)

daySelector :: Int -> String -> String
daySelector d | d == 1 = solve_day_1
              | d == 2 = (++ "48484")
              | otherwise = id


solve_day_1 :: String -> String
solve_day_1 s = solve_day_1_part_1 s ++ "\n" ++ solve_day_1_part_2 s


solve_day_1_part_1 :: String -> String
solve_day_1_part_1  = show . sum . map compare . windows 2 . toIntList . splitAll '\n'

solve_day_1_part_2 :: String -> String
solve_day_1_part_2  = show . sum . map compare . windows 2 . map sum . windows 3 . toIntList . splitAll '\n'


merge :: (String, String) -> String
merge = uncurry (++)

toIntList :: [String] -> [Int]
toIntList [] = []
toIntList (x:xs) = case readMaybe x of
                Just a -> a : toIntList xs
                Nothing -> toIntList xs

compare :: [Int] -> Int
compare [x, y] | x < y = 1
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
