module Day8 (solve) where
import Common (toIntList, splitAll)

solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 = const "Part One"

solve_day_part_2 :: String -> String
solve_day_part_2 = const "Part Two"
