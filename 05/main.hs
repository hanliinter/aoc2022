import Data.List

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where
                       (w,xs'') = break p xs'


moveOne :: ([a],[a]) -> ([a],[a])
moveOne ((a:as), bs) = (as, a:bs)

move :: Int -> ([a],[a]) -> ([a],[a])
move n pair | n == 0 = pair
            | otherwise = move (n-1) $ moveOne pair 


move9001 :: Int -> ([a],[a]) -> ([a],[a])
move9001 n (as,bs) = (drop n as, (take n as) ++ bs) 
            



replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = let (before,_:after) = splitAt n xs in
                      before ++ (x:after)


craneMove :: [String] -> [Int] -> [String]
craneMove crates command@(qty:src:tgt:_) =  let srcCrate = crates !! (src -1)
                                                tgtCrate = crates !! (tgt -1)
                                                (srcCrate', tgtCrate') = move9001 qty (srcCrate,tgtCrate)
                                                crates' = replaceAt (src-1) srcCrate' crates
                                                crates'' = replaceAt (tgt-1) tgtCrate' crates'
                                            in
                                              crates''

extractCommand :: String -> [Int]
extractCommand xs = let (_:qty:_:src:_:tgt:_) = words xs in
                      map read [qty,src,tgt]


ws = ["    [D]    ","[N] [C]    ","[Z] [M] [P]"," 1   2   3 "]
                      
replaceBracket = map (\c -> if c == '[' || c == ']' then ' ' else c)

prepare_part1 :: String -> ([String], [[Int]])
prepare_part1 xs = let (m:c:_) = wordsWhile (== "") $ lines xs
                       m' = transpose $ map replaceBracket m
                       crates =filter (/="") $ map (filter (/=' ')) m'
                       commands = map extractCommand c
                   in
                     (crates,commands)
solve :: [String] -> [[Int]] -> String 
solve crates commands = let finalCrates = foldl craneMove crates commands in
                                map head finalCrates


main :: IO ()
main = do
  file <- readFile "input.txt"
  let (crates, commands) = prepare_part1 file
  putStrLn $ solve crates commands
