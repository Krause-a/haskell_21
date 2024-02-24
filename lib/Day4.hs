module Day4 (solve) where
import Common (splitAll, toIntList)
import Data.List (transpose, minimumBy, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)


solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 = show . (snd . minimumBy (compare `on` fst)) . getScoreDepths

solve_day_part_2 :: String -> String
solve_day_part_2 = show . (snd . maximumBy (compare `on` fst)) . getScoreDepths



getScoreDepths :: String -> [(Int, Int)]
getScoreDepths xs = let
    draw = (toIntList . splitAll ',' . head . lines) xs
    bingos = boards . drop 1 $ lines xs
    in playBoards draw bingos

playBoards :: [Int]
           -> [[[Maybe Int]]]
           -> [(Int, Int)]
playBoards _ [] = []
playBoards draws (x:xs) = winBoard draws x : playBoards draws xs

winBoard :: [Int]           -- Draws
         -> [[Maybe Int]]   -- Board
         -> (Int, Int)      -- (Steps, Score)
winBoard = _winBoard 0

_winBoard :: Int            -- Current Depth
          -> [Int]          -- Tail Draws
          -> [[Maybe Int]]  -- Board
          -> (Int, Int)     -- (Steps, Score)
_winBoard depth draws board = let
    currentDraw = head draws
    nextBoard = map (map (rowOp currentDraw)) board
    in if winCheck  nextBoard then (depth + 1, scoreBoard currentDraw (
    -- trace ("Final draw: " ++ show currentDraw ++ "\n" ++ print_2D nextBoard)
        nextBoard)) else _winBoard (depth+1) (tail draws) nextBoard
    where
        rowOp :: (Eq a) => a -> Maybe a -> Maybe a
        rowOp check ele = case ele of
            Just val -> if check == val then Nothing else Just val
            Nothing -> Nothing
        anyNothing :: [Maybe a] -> Bool
        anyNothing = foldl (\acc x -> case x of Just _ -> acc ; Nothing -> True) False
        winCheck [] = error ("No board! " ++ show depth)
        winCheck board = let
            winRows = map (foldl (\acc x -> case x of Just val -> Just val ; Nothing -> acc) Nothing ) board
            winCols = map (foldl (\acc x -> case x of Just val -> Just val ; Nothing -> acc) Nothing ) $ transpose board
            in anyNothing winRows || anyNothing winCols
        scoreBoard :: Int -> [[Maybe Int]] -> Int
        scoreBoard d b = d * let
            summed = foldl (\acc x -> acc + foldl (\ycc y -> ycc + fromMaybe 0 y) 0 x) 0 b
            in --trace ("Sum: " ++ show summed)
                summed



boards :: [String] -> [[[Maybe Int]]]
boards [] = []
boards xs = (bingoBoard . take 5 . drop 1) xs : boards (drop 6 xs)

bingoBoard :: [String] -> [[Maybe Int]]
bingoBoard xs = map makeRow xs
    where makeRow = map Just . toIntList . words

print_2D :: (Show a) => [[a]] -> String
print_2D [] = ""
print_2D (x:xs) = show x ++ "\n" ++ print_2D xs


-- Here is the idea. Create 2D lists of Maybe Int.
-- For a winning row or column I can check for any Just or all Nothing.
-- Getting the sum at the end would be very easy.
-- is good idea!
