import Data.List

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


main :: IO ()
main = do
  file <- readFile "sample.input"
  let input = wordsWhile (==[]) $ lines file
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input

part1 :: [[String]] -> Int
part1 input =  maximum (map sum (map (map read) input))




part2 :: [[String]] -> Int
part2 input =  sum $ take 3 $ sortBy (\a b -> if a < b then GT else LT) (map sum (map (map read) input))
