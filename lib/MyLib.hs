module MyLib (daySelector) where
import qualified Day1
import qualified Day2
import qualified Day3

daySelector :: Int -> String -> String
daySelector d | d == 1 = Day1.solve
              | d == 2 = Day2.solve
              | d == 3 = Day3.solve
              | otherwise = id

