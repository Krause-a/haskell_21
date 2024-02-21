module Day1 (solve) where
import Common

solve :: String -> String
solve s = solve_day_1_part_1 s ++ "\n" ++ solve_day_1_part_2 s

solve_day_1_part_1 :: String -> String
solve_day_1_part_1  = show . sum . map compareDayOne . windows 2 . toIntList . splitAll '\n'

solve_day_1_part_2 :: String -> String
solve_day_1_part_2  = show . sum . map compareDayOne . windows 2 . map sum . windows 3 . toIntList . splitAll '\n'


compareDayOne :: [Int] -> Int
compareDayOne [x, y] | x < y = 1
               | otherwise = 0
