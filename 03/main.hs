
part1 :: [String] -> Int
part1 xs = sum $ map go xs
  where go:: String -> Int
        go x = let len = length x `div` 2
                   as = take len x
                   bs = drop len x
                   cs = [a | a <- as, a `elem` bs]
                in
                 if length cs == 0 then 0 else atoi $ head cs


part2 :: [String] -> Int
part2 xs = sum result
  where result = splitForGroup xs


splitForGroup :: [String] -> [Int]
splitForGroup xs | length xs > 3 = let (g,rest) = splitAt 3 xs in
                                       (calc g) : splitForGroup rest
                 | length xs == 3 = [calc xs]
                 | otherwise = error "input invalid"


calc :: [String] -> Int
calc (a:b:c:_) = let result = [r | r <- a , r `elem` b , r `elem` c] in
                 atoi $ head result
                 
atoi :: Char -> Int
atoi c = case lookup c dict of
           Just i -> i
           Nothing -> error "wrong input"
          where dict = zip (['a'..'z'] ++ ['A'..'Z']) [1..52]
        
main :: IO ()
main = do
  file <- readFile "sample.txt"
  putStrLn $ show $ part1 $ lines file
  putStrLn $ show $ part2 $ lines file
