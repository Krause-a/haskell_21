module Common (
    windows,
    splitAll,
    toIntList,
) where
import Text.Read (readMaybe)

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows size xs
            | length xs >= size = take size xs : windows size (drop 1 xs)
            | otherwise = []

split :: Char -> String -> (String, String)
split _ [] = ("", "")
split sep (x:xs) 
                | x == sep = ("", xs)
                | x /= sep = let (first, second) = split sep xs in (x : first, second)

splitAll :: Char -> String -> [String]
splitAll _ [] = []
splitAll sep xs = let (first, second) = split sep xs in first : splitAll sep second

toIntList :: [String] -> [Int]
toIntList [] = []
toIntList (x:xs) = case readMaybe x of
                Just a -> a : toIntList xs
                Nothing -> toIntList xs
