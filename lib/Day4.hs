module Day4 (solve) where


solve :: String -> String
solve s = solve_day_part_1 s ++ "\n" ++ solve_day_part_2 s

solve_day_part_1 :: String -> String
solve_day_part_1 = id

solve_day_part_2 :: String -> String
solve_day_part_2 = id




-- Here is the idea. Create 2D lists of Maybe Int.
-- For a winning row or column I can check for any Just or all Nothing.
-- Getting the sum at the end would be very easy.
-- is good idea!
