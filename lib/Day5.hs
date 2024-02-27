module Day5 (solve) where
import Common (toIntList, splitAll)
import qualified Data.Map as Map
import Debug.Trace (trace)

type Point = (Int, Int)


solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 = show . foldr (\x acc -> if x > 1 then 1 + acc else acc) 0 . intoMap . map (makeLine' CardinalOnly . parseLine) . splitAll '\n'

solve_day_part_2 :: String -> String
solve_day_part_2 = show . foldr (\x acc -> if x > 1 then 1 + acc else acc) 0 . intoMap . map (makeLine' IncludeOrdinal . parseLine) . splitAll '\n'

intoMap :: [[Point]] -> Map.Map Point Int
intoMap lines = let
    m = foldr (\p -> Map.insertWith (+) p 1) Map.empty
    flatLines = concat lines
    in m flatLines

parseLine :: String -> (Point, Point)
parseLine s = let
    a = words s
    f = head a
    l = last a
    in (parsePoint f, parsePoint l)
    where
        parsePoint :: String -> Point
        parsePoint s = case (toIntList . splitAll ',') s of
            [x, y] -> (x, y)

data Cardinals = CardinalOnly | IncludeOrdinal deriving (Show, Eq)

makeLine :: Cardinals -> Point -> Point -> [Point]
makeLine' card (p0, p1) = makeLine card p0 p1
makeLine card (x0, y0) (x1, y1)
    | x0 /= x1 && y0 /= y1 && card == CardinalOnly = []
    | x0 == x1 && y0 == y1 = [(x0, y0)]
    | otherwise = let
        yInc = getInc y0 y1
        xInc = getInc x0 x1
        in makeLine card (xInc x0, yInc y0) (x1, y1) ++ [(x0, y0)]
    where
        getInc :: Int -> Int -> Int -> Int
        getInc a b = case compare a b of
            LT -> succ
            EQ -> id
            GT -> pred
