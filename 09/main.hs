import Control.Monad
import Control.Monad.Trans.Writer
-- assume that start point is (0,0)

data Direction = Up | Right | Down | Left
type Command = (Direction,Int)
type Index = (Int,Int)

stepOver :: (Index,Index) -> [Direction] -> Writer [Index] (Index,Index)
stepOver pair directions  = foldM stepOne pair directions

stepOne :: (Index,Index) -> Direction -> Writer [Index] (Index,Index)
stepOne pair dir  = do
   case dir of
     Up -> stepUp pair
     Down -> stepDown pair
     Main.Right -> stepRight pair
     Main.Left -> stepLeft pair

stepUp ::(Index, Index) -> Writer [Index] (Index,Index)
stepUp (h@(hx,hy),t@(tx,ty)) = let h' = (hx+1, hy) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Horizontal  -> return (h', t)
                                   Vertical    -> if hx > tx then tell [(tx+1,ty)] >> return (h',(tx+1,ty))
                                                             else return (h',t)
                                   NE          -> tell [(tx+1, ty+1)] >> return (h' ,(tx+1,ty+1))
                                   SE          -> return (h',t)
                                   NW          -> tell [(tx+1, ty-1)] >> return (h',(tx+1,ty-1))
                                   SW          -> return (h',t)



stepDown ::(Index, Index) -> Writer [Index] (Index,Index)
stepDown (h@(hx,hy),t@(tx,ty)) = let h' = (hx-1, hy) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Horizontal  -> return (h', t)
                                   Vertical    -> if hx < tx then tell [(tx-1,ty)] >> return (h',(tx-1,ty))
                                                             else return (h',t)
                                   NE          -> return (h' ,t)
                                   SE          -> tell [(tx-1,ty+1)]  >> return (h',(tx-1,ty+1))
                                   NW          -> return (h', t)
                                   SW          -> tell [(tx-1,ty-1)] >> return (h',(tx-1,ty-1))


stepLeft ::(Index, Index) -> Writer [Index] (Index,Index)
stepLeft (h@(hx,hy),t@(tx,ty)) = let h' = (hx, hy-1) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Vertical  -> return (h', t)
                                   Horizontal    -> if hy < ty then tell [(tx,ty-1)] >> return (h',(tx,ty-1))
                                                             else return (h',t)
                                   NE          -> return (h' ,t)
                                   SE          -> return (h', t)
                                   NW          -> tell [(tx+1, ty-1)] >> return (h', (tx+1,ty-1))
                                   SW          -> tell [(tx-1,ty-1)] >> return (h',(tx-1,ty-1))


stepRight ::(Index, Index) -> Writer [Index] (Index,Index)
stepRight (h@(hx,hy),t@(tx,ty)) = let h' = (hx, hy+1) in
                                 case touchType h t of
                                   Overlapping -> return (h', t)
                                   Vertical  -> return (h', t)
                                   Horizontal    -> if hy > ty then tell [(tx,ty+1)] >> return (h',(tx,ty+1))
                                                             else return (h',t)
                                   NW          -> return (h' ,t)
                                   SW          -> return (h', t)
                                   NE          -> tell [(tx+1, ty+1)] >> return (h', (tx+1,ty+1))
                                   SE          -> tell [(tx-1,ty+1)] >> return (h',(tx-1,ty+1))


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
                      |otherwise = error "should not happen"


stringToDirection:: String -> Direction
stringToDirection "U" = Up
stringToDirection "R" = Main.Right
stringToDirection "L" = Main.Left
stringToDirection "D" = Down

lineToInstruction :: String -> [Direction]
lineToInstruction xs = let (d:n:_) = words xs
                           n' = read n
                           d' = stringToDirection d
                       in
                         take n' repeat d'


handleInput :: String -> [Direction]
handleInput xs =map (\(d:))  map words $lines xs
