import Control.Monad
import Control.Monad.Writer
--import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.List
-- assume that start point is (0,0)

data Direction = U | R | D | L | UR | UL | DL | DR | NOP deriving (Show)
type Command = (Direction,Int)
type Index = (Int,Int)

changePoint :: Index -> Direction -> Index
changePoint (x,y) U = (x+1,y)
changePoint (x,y) R = (x,y+1)
changePoint (x,y) D = (x-1,y)
changePoint (x,y) L = (x,y-1)
changePoint (x,y) UR = (x+1,y+1)
changePoint (x,y) UL = (x+1,y-1)
changePoint (x,y) DL = (x-1,y-1)
changePoint (x,y) DR = (x-1,y+1)
changePoint _ NOP = error "should not happen"


stepOver :: (Index,Index) -> [Direction] -> Writer [Index] (Index,Index)
stepOver pair directions  = foldM stepOneM pair directions


-- stepOne2 :: [Index] -> Direction -> Writer [Index] [Index]
-- stepOne2 ts d = go ts [] d
--   where go (t2:t:[]) result d = stepOnePart2 (t2,t)

--stepOnePart2 (Index,Index) -> Direction -> (Index,Index,Direction)
--stepOnePart2 (curr,next) 

stepUp ::(Index, Index) -> (Index,Index,Direction)
stepUp (h@(hx,hy),t@(tx,ty)) = let h' = changePoint h U in
                                 case touchType h t of
                                   Overlapping -> (h', t, NOP)
                                   Horizontal  -> (h', t, NOP)
                                   Vertical    -> if hx > tx then (h',(changePoint t U), U)
                                                             else (h',t,NOP)
                                   NE          -> (h' ,(changePoint t UR),UR)
                                   SE          -> (h',t,NOP)
                                   NW          -> (h',(changePoint t UL),UL)
                                   SW          -> (h',t,NOP)


stepDown ::(Index, Index) -> (Index,Index,Direction)
stepDown (h@(hx,hy),t@(tx,ty)) = let h' = changePoint h D in
                                 case touchType h t of
                                   Overlapping -> (h', t, NOP)
                                   Horizontal  -> (h', t, NOP)
                                   Vertical    -> if hx < tx then (h',(changePoint t D), D)
                                                             else (h',t,NOP)
                                   NE          -> (h',t,NOP)
                                   SE          -> (h',(changePoint t DR),DR)
                                   NW          -> (h',t,NOP)
                                   SW          -> (h',(changePoint t DL),DL)

stepLeft ::(Index, Index) -> (Index,Index,Direction)
stepLeft (h@(hx,hy),t@(tx,ty)) = let h' = changePoint h L in
                                 case touchType h t of
                                   Overlapping -> (h', t, NOP)
                                   Horizontal  -> if hy < ty then (h', (changePoint t L),L)
                                                             else (h,t,NOP)
                                   Vertical    -> (h',t,NOP)
                                   NE          -> (h',t,NOP)
                                   SE          -> (h',(changePoint t DR),DR)
                                   NW          -> (h',t,NOP)
                                   SW          -> (h',(changePoint t DL),DL)

stepRight ::(Index, Index) -> (Index,Index,Direction)
stepRight (h@(hx,hy),t@(tx,ty)) = let h' = changePoint h R in
                                 case touchType h t of
                                   Overlapping -> (h', t, NOP)
                                   Horizontal  -> if hy > ty then (h', (changePoint t R),R)
                                                             else (h,t,NOP)
                                   Vertical    -> (h',t,NOP)
                                   NE          -> (h',t,NOP)
                                   SE          -> (h',(changePoint t DR),DR)
                                   NW          -> (h',t,NOP)
                                   SW          -> (h',(changePoint t DL),DL)


stepUpLeft ::(Index, Index) -> (Index,Index,Direction)
stepUpLeft (h@(hx,hy),t@(tx,ty)) = let h' = (hx+1, hy) in
                                 case touchType h t of
                                   Overlapping -> (h', t, NOP)
                                   Horizontal  -> if hy < ty then (h', (changePoint t L),L)
                                                             else (h,t,NOP)
                                   Vertical    -> (h',t,NOP)
                                   NE          -> (h',t,NOP)
                                   SE          -> (h',(changePoint t DR),DR)
                                   NW          -> (h',t,NOP)
                                   SW          -> (h',(changePoint t DL),DL)


stepOneM :: (Index,Index) -> Direction -> Writer [Index] (Index,Index)
stepOneM pair dir  = do
   case dir of
     U -> stepUpM pair
     D -> stepDownM pair
     R -> stepRightM pair
     L -> stepLeftM pair

stepUpM ::(Index, Index) -> Writer [Index] (Index,Index)
stepUpM (h@(hx,hy),t@(tx,ty)) = let h' = (hx+1, hy) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Horizontal  -> return (h', t)
                                   Vertical    -> if hx > tx then tell [(changePoint t U)] >> return (h',(changePoint t U))
                                                             else return (h',t)
                                   NE          -> tell [(changePoint t UR)] >> return (h' ,(changePoint t UR))
                                   SE          -> return (h',t)
                                   NW          -> tell [(changePoint t UL)] >> return (h',(changePoint t UL))
                                   SW          -> return (h',t)



stepDownM ::(Index, Index) -> Writer [Index] (Index,Index)
stepDownM (h@(hx,hy),t@(tx,ty)) = let h' = (hx-1, hy) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Horizontal  -> return (h', t)
                                   Vertical    -> if hx < tx then tell [(changePoint t D)] >> return (h',(changePoint t D))
                                                             else return (h',t)
                                   NE          -> return (h' ,t)
                                   SE          -> tell [(changePoint t DR)]  >> return (h',(changePoint t DR))
                                   NW          -> return (h', t)
                                   SW          -> tell [(changePoint t DL)] >> return (h',(changePoint t DL))


stepLeftM ::(Index, Index) -> Writer [Index] (Index,Index)
stepLeftM (h@(hx,hy),t@(tx,ty)) = let h' = (hx, hy-1) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Vertical  -> return (h', t)
                                   Horizontal    -> if hy < ty then tell [(changePoint t L)] >> return (h',(changePoint t L))
                                                             else return (h',t)
                                   NE          -> return (h' ,t)
                                   SE          -> return (h', t)
                                   NW          -> tell [(changePoint t UL)] >> return (h', (changePoint t UL))
                                   SW          -> tell [(changePoint t DL)] >> return (h',(changePoint t DL))


stepRightM ::(Index, Index) -> Writer [Index] (Index,Index)
stepRightM (h@(hx,hy),t@(tx,ty)) = let h' = (hx, hy+1) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Vertical  -> return (h', t)
                                   Horizontal    -> if hy > ty then tell [(changePoint t R)] >> return (h',(changePoint t R))
                                                             else return (h',t)
                                   NW          -> return (h' ,t)
                                   SW          -> return (h', t)
                                   NE          -> tell [(changePoint t UR)] >> return (h', (changePoint t UR))
                                   SE          -> tell [(changePoint t DR)] >> return (h',(changePoint t DR))


data TouchType = Horizontal | Vertical  | Overlapping | NE | NW | SE | SW deriving (Show)
                                                                       -- NE -> H is at the NE of T
touchType :: Index -> Index -> TouchType
touchType (a,b) (c,d) | a == c && b == d = Overlapping
                      | a == c && abs(b - d) == 1 = Horizontal
                      | b == d && abs(a - c) == 1 = Vertical
                      | a - c == 1 &&  b - d == 1 = NE
                      | a - c == -1 && b -d == 1 = SE
                      | a -c == -1 && b - d == -1 = SW
                      | a - c == 1 && b - d == -1 = NW
                      |otherwise = error $ "should not happen" ++ show a ++ " " ++ show b


stringToDirection:: String -> Direction
stringToDirection "U" = U
stringToDirection "R" = R
stringToDirection "L" = L
stringToDirection "D" = D

lineToInstruction :: String -> [Direction]
lineToInstruction xs = let (d:n:_) = words xs
                           n' = read n
                           d' = stringToDirection d
                       in
                         take n' $ repeat d'


handleInput :: String -> [Direction]
handleInput xs =concat $ map lineToInstruction $lines xs

main = do
  input <- readFile "input.txt"
  let (_,visited) =  runWriter $ stepOver ((0,0),(0,0)) $ handleInput input
  putStrLn $ show $ length $ nub ((0,0):visited)
