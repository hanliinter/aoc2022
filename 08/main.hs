import Data.Char
import Data.List

type Trees = [[Int]]
type Indices = (Int, Int)

type ITrees =[ [(Indices,Int)] ]


fromMap :: String -> Trees
fromMap xs = map (map digitToInt) $ lines xs

annotateTrees :: Trees -> ITrees
annotateTrees hs = let temp = map ( \(i, xs) -> (i, zip [0..] xs)) $ zip [0..] hs in
                     map (\(i,ts) -> map (\(j,h) -> ((i,j),h)) ts ) temp


firstBig :: [Int] -> [Int]
firstBig (x:xs) = go xs [x]
    where
    go (a:[]) result  = reverse  (a:result)
    go (a:bs) result@(x:_) = if a <= x then go bs result
                                      else go bs (a:result)
         
countVisibleInRowFromFront :: [(Indices,Int)] -> [(Indices,Int)]
countVisibleInRowFromFront (x:xs) = go xs [x]
  where --go [] result = reverse result
        go (a:[]) result = reverse (a:result)
        go (a:bs) result@(x:_) = if (snd a) <= (snd x) then go bs result
                                                       else go bs (a:result)


countRows :: [[(Indices,Int)]] -> [[(Indices,Int)]]
countRows aTree = let leftToRight = map countVisibleInRowFromFront aTree
                      rightToleft = map (countVisibleInRowFromFront . reverse) aTree
                      combined    = zipWith (++) leftToRight rightToleft
                      result      =  map nub combined
                  in
                    result

removeFence ::Int-> [[(Indices,Int)]] -> [[(Indices,Int)]]
removeFence n xs = map (filter (\((x,y),_) -> x == 0 || y == 0 || x == n || y == n)) xs
             

main = do
  file <- readFile "input.txt"
  let aTree = annotateTrees $ fromMap file
      passOne =  concat $countRows aTree
      passTwo =  concat $ transpose $ countRows $ transpose aTree
      result =  nub $ passOne ++ passTwo
  putStrLn $ show $ length $ result
