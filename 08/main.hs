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


calcDistance :: [Int] -> Int -> (Int,Int)
calcDistance xs i = let (left,x:right) = splitAt i xs
                        leftDistance = calcBackword left x
                        rightDistance = calcForward right x
                    in
                      (leftDistance,rightDistance)

calcBackword :: [Int] -> Int -> Int
calcBackword left x = calcForward (reverse left) x
  
calcForward :: [Int] -> Int -> Int
calcForward right x = go right x 1
                 where go [] x n = n -1 
                       go (a:as) x n = if a < x then go as x (n+1)
                                                else n 
                    
calcScore :: Trees -> Indices -> Int
calcScore trees (x,y) = let row = trees !! x
                            (l,r) = calcDistance row y
                            col = (transpose trees) !! y
                            (u,d) = calcDistance col x
                         in
                          product [l,r,u,d]


main = do
  file <- readFile "input.txt"
  let 
      trees  = fromMap file
      aTree = annotateTrees trees
      passOne =  concat $countRows aTree
      passTwo =  concat $ transpose $ countRows $ transpose aTree
      result =  nub $ passOne ++ passTwo
      socres = map (\(index,_) -> calcScore trees index ) result
  putStrLn $ show $ length $ result
  putStrLn $ show $ maximum socres
