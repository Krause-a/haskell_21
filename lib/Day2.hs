module Day2 (solve) where

solve :: String -> String
solve s = solve_day_2_part_1 s ++ "\n" ++ solve_day_2_part_2 s

solve_day_2_part_1 :: String -> String
solve_day_2_part_1 = show . uncurry (*) . foldl submarineFlatRoute (0, 0) . map words . lines

solve_day_2_part_2 :: String -> String
solve_day_2_part_2 = show . (\(_, x, y) -> x * y) . foldl submarineAimRoute (0, 0, 0) . map words . lines


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
