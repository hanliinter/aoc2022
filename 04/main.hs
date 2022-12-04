type Interval = (Int, Int)


wordsWhile :: (a-> Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                      where
                      (w,xs'') = break p xs'


contains :: Interval -> Interval -> Bool
contains (a,b) (c,d) = if a <= c && b>=d then True
                                         else if c<=a && d>=b then True
                                                              else False


buildInteval :: String -> Interval
buildInteval xs = let (a:b:_) = wordsWhile (=='-') xs in
                    (read a, read b)

solve ::(Interval->Interval->Bool) -> String -> Bool
solve f xs = let (a:b:_) = wordsWhile (==',') xs in
                    f (buildInteval a) (buildInteval b)


part1 :: [String] -> Int
part1 xs = length $ filter (== True) $  map (solve contains)  xs


overlapped :: Interval -> Interval -> Bool
overlapped aI@(a,_) bI@(b,_) = if a <=b then overlap_direction aI bI
                                      else overlap_direction bI aI

overlap_direction :: Interval -> Interval -> Bool
overlap_direction (a,b) (c,d) = if (b< c) then False
                                          else True

part2 :: [String] -> Int
part2 xs = length $ filter (== True) $  map (solve overlapped)  xs



main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ show $ part1 $ lines content
  putStrLn $ show $ part2 $ lines content
