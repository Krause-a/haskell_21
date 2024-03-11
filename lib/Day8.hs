module Day8 (solve) where
import Common (toIntList, splitAll)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Data.List (sortBy)
import Debug.Trace (trace)

solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 s = let
    lines = splitAll '\n' s
    processed = map processLineday1 lines
    some = sum $ map (foldr (\x acc -> acc + if x `elem` [2,3,4,7] then 1 else 0) 0) processed
    in show some

solve_day_part_2 :: String -> String
solve_day_part_2 s = let
    lines = splitAll '\n' s
    processed = map lineByLine lines
    in show processed

lineByLine :: String -> [Int]
lineByLine line = let
    (input, output) = processLine line
    ss = sortBy (compare `on` length) $ getKnownSevenSegs input
    -- Now it is lined up by length. All unique segments groups will be at a known index
    what = head $ trace (show ss) ss
    why = head $ trace (show what) what

    in [seglength why]



processLineday1 line = let
    parse1 = splitAll '|' line
    output = drop 1 $ splitAll ' ' (last parse1)
    put = map length output
    in put


processLine :: String -> ([String], [String])
processLine line = let
    parse1 = splitAll '|' line
    input = splitAll ' ' (head parse1)
    output = drop 1 $ splitAll ' ' (parse1 !! 1)
    in (input, output)

getKnownSevenSegs :: [String] -> [UnknownSegGroup]
getKnownSevenSegs = mapMaybe inputToSegments



data Seg a = T a | M a | B a | TR a | TL a | BR a | BL a

seglength :: UnknownSeg -> Int
seglength (T a)  = Prelude.length a
seglength (M a)  = Prelude.length a
seglength (B a)  = Prelude.length a
seglength (TR a) = Prelude.length a
seglength (TL a) = Prelude.length a
seglength (BR a) = Prelude.length a
seglength (BL a) = Prelude.length a

instance Show a => Show (Seg a) where
    show (T a) = "T: " ++ show a
    show (M a) = "M: " ++ show a
    show (B a) = "B: " ++ show a
    show (TR a) = "TR: " ++ show a
    show (TL a) = "TL: " ++ show a
    show (BR a) = "BR: " ++ show a
    show (BL a) = "BL: " ++ show a

instance Eq (Seg a) where
    (T _) == (T _) = True
    (M _) == (M _) = True
    (B _) == (B _) = True
    (TR _) == (TR _) = True
    (TL _) == (TL _) = True
    (BR _) == (BR _) = True
    (BL _) == (BL _) = True
    _ == _ = False


type KnownSeg = Seg Char
type UnknownSeg = Seg String
type UnknownSegGroup = [UnknownSeg]

inputToSegments :: String -> Maybe UnknownSegGroup
inputToSegments input
    | l == 2 = Just [TR input, BR input]
    | l == 3 = Just [T input, TR input, BR input]
    | l == 4 = Just [TL input, M input, TR input, BR input]
    | l == 7 = Just [T input, M input, B input, TR input, TL input, BR input, BL input]
    | otherwise = Nothing
    where l = length input


subtract :: UnknownSegGroup -> UnknownSegGroup -> UnknownSegGroup
subtract [] _ = error "BAH"
subtract _ [] = error "BAH"
subtract x y = diff
    where
        xIn = extract $ head x
        yIn = extract $ head y
        xIsLonger = length xIn > length yIn
        string_longer = if xIsLonger then xIn else yIn
        string_shorter = if xIsLonger then yIn else xIn
        string_diff = string_longer \\ string_shorter
        longer = if xIsLonger then x else y
        shorter = if xIsLonger then y else x
        diff = longer \\ shorter



(\\) :: (Eq a) => [a] -> [a] -> [a]
x \\ y = filter (`elem` y) x




extract :: Seg a -> a
extract (T x) = x
extract (M x) = x
extract (B x) = x
extract (TR x) = x
extract (TL x) = x
extract (BR x) = x
extract (BL x) = x
