module MyLib (daySelector) where
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

daySelector :: Int -> String -> String
daySelector d | d == 1 = Day1.solve
              | d == 2 = Day2.solve
              | d == 3 = Day3.solve
              | d == 4 = Day4.solve
              | d == 5 = Day5.solve
              | otherwise = ("OTHERWISE"++) . (++"OTHERWISE")

