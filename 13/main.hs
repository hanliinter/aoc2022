{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Debug.Trace
import Data.Ord
import Data.List
data Packet = List [Packet]
            | Val Int
            deriving (Show,Eq)

instance Ord Packet where
  compare = compareP


showPacket :: Packet -> String
showPacket (List []) = "[]"
showPacket (Val a)   = show a
showPacket (List [p]) = "[" ++ showPacket p ++ "]"
showPacket (List (p:ps)) = "[" ++ showPacket p ++ showPacketList ps
  where showPacketList [] = "]"
        showPacketList (x:xs) = "," ++ showPacket x ++ showPacketList xs
  
compareP :: Packet -> Packet -> Ordering
compareP  (Val a) (Val b) = Prelude.compare a b
compareP  (List as'@(a:as)) (List bs'@(b:bs)) =  case compareP ( a) (b) of
                                                    EQ -> compareP ((List as)) ( List bs)
                                                    other -> other
                                                
compareP  (List []) (List (b:_)) = LT
compareP  (List (a:_))(List []) = GT
compareP (List []) (List []) = EQ
compareP  (List a) (Val b) = compareP (List a) (List [Val b])
compareP  (Val a) (List b) = compareP (List [Val a]) (List b)

val :: Stream s m Char => ParsecT s u m Packet
val = many (digit) >>= \num -> return $ Val (read num)

emptyList :: Stream s m Char => ParsecT s u m Packet
emptyList = string "[]" *> pure (List [])

--innerList :: Stream s m Char => ParsecT s u m Packet
--innerList = string "[" *> (sepBy val (string ",") ) <* string "]"  >>= \l -> return $ List l

item :: Stream s m Char => ParsecT s u m Packet
item =   (try emptyList) <|>  try list <|> try val

singletonList :: Stream s m Char => ParsecT s u m Packet
singletonList =  string "[" *> item  <* string "]" >>= \p -> return (List [p])

multiItemList :: Stream s m Char => ParsecT s u m Packet
multiItemList =  string "[" *> (sepBy item (string ",")) <* string "]" >>= \p -> return $ List p


list :: Stream s m Char => ParsecT s u m Packet
list = try emptyList <|> try singletonList  <|>try multiItemList


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                      where (w,xs'') = break p xs'

parsePacket :: String -> Packet
parsePacket xs = case parse list "" xs of
                   Left _ -> error "parse failed"
                   Right p -> p

parsePair :: [String] -> (Packet,Packet)
parsePair (a:b:_) = (parsePacket a, parsePacket b)

testA = List [List [Val 1],List [Val 2,Val 3,Val 4]]
testB = List [List [Val 1],Val 4]


getPacket :: FilePath -> IO [(Packet,Packet)]
getPacket file = do
  content <- readFile file
  --map parsePackets
  let rawStrings = wordsWhile (=="") $ lines content
  let pairs = map parsePair rawStrings
  return pairs

--(List [List [Val 1],List [Val 2,Val 3,Val 4]],List [List [Val 1],Val 4])
part1::FilePath -> IO Int
part1 file = do
  pairs <- getPacket file
  let result = map (uncurry compareP) pairs
  --putStrLn $show result
  let answer = sum $ map fst $ filter (\(i,v) -> v /=GT) $ zip [1..] result
  putStrLn $ show answer
  return answer


part2 :: FilePath -> IO ()
part2 file = do
  pairs <-getPacket file
  let packets = concatMap (\(a,b) -> [a,b]) pairs
  let dividerA = List [List [Val 2]]
  let dividerB = List [List [Val 6]]
  let answerList = zip [1..] $ sort (dividerA:dividerB:packets)
  let answer = product $ map fst $ filter(\(_,v)-> v == dividerA || v == dividerB) answerList
  putStrLn $ show answer

