module Main where
import System.Environment
import System.IO

import MyLib (daySelector)

test :: String
test = "test_inputs/"
challenge :: String
challenge = "challenge_inputs/"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, _] -> do
      fileHandle <- openFile (challenge ++ day) ReadMode
      fileContents <- hGetContents fileHandle
      putStrLn $ MyLib.daySelector (read day :: Int) fileContents
      hClose fileHandle
    [day] -> do
      fileHandle <- openFile (test ++ day) ReadMode
      fileContents <- hGetContents fileHandle
      putStrLn $ MyLib.daySelector (read day :: Int) fileContents
      hClose fileHandle
    _ArgError -> putStrLn "ERROR"
