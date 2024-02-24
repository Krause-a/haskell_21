module Day5 (solve) where



solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 = (++ "ONE")

solve_day_part_2 :: String -> String
solve_day_part_2 = (++ "TWO")

