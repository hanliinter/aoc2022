import Data.List

findMarker :: [Char] -> Int
findMarker xs@(c:cs) = if (length $ nub $ take 4 xs) ==  4 then 4
                                           else 1 + findMarker cs



findMessage :: [Char] -> Int
findMessage xs@(c:cs) = if (length $ nub $ take 14 xs) ==  14 then 14
                                           else 1 + findMessage cs



main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn $ show $ findMessage file
