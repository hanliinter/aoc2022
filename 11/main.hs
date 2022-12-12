{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State
import Data.List
--import Text.Parsec
-- I don't like the hard code solution but it works now

--command :: Stream s m Char => ParsecT s u m String
--command = string "xss"
wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where
                       (w,xs'') = break p xs'


data Monkey = Monkey {
                       items :: [Int]
                     , operation :: (Int  -> Int)
                     , divisible :: Int
                     , trueBranch :: Int
                     , falseBranch :: Int
                     , count :: Int
                     }

instance Show Monkey where
  show  (Monkey _items _ divisible _ _ _) = "(" ++ show _items ++ "," ++ show divisible ++ ")"

loadMonkeys :: String -> [Monkey]
loadMonkeys xs = let xs' = wordsWhile (=="") $ lines xs in
                   map loadMonkey xs'
  
loadMonkey :: [String] -> Monkey
loadMonkey (_:a:b:c:d:e:[]) = Monkey {
                                        items = readItems a
                                       ,operation = readOper b
                                       ,divisible = readLastNum c
                                       ,trueBranch = readLastNum d
                                       , falseBranch = readLastNum e
                                       , count = 0
                                       }
loadMonkey _ = error "malformat"





readItems ::String ->[Int]
readItems xs = let (_:sub:_) = wordsWhile (==':') xs
                   ns = wordsWhile(==',') sub
               in
                 map read ns

readOper :: String -> (Int->Int)
readOper xs = let (_:sub:_) = wordsWhile (=='=') xs
                  ops = words sub
              in
                oper ops

readLastNum :: String -> Int
readLastNum xs = read $ last $ words xs
                
oper :: [String] -> (Int  -> Int)
oper (operant1:op:operant2:_) = if operant1 == operant2 then case op of
                                                                    "*" -> (^2)
                                                                    "+" -> (*2)
                                                              else
                                                                  case op of
                                                                    "*" -> (* (read operant2))
                                                                    "+" -> (+ (read operant2))


-- testMonkey0 = Monkey{
--                      items = [79,98]
--                     ,operation = (*19)
--                     ,divisible = 23
--                     ,trueBranch = 1
--                     ,falseBranch = 1
--                     ,count =0
--                     }

-- testMonkey1 = Monkey{
--                      items = [54,65]
--                     ,operation = (+619)
--                     ,divisible = 19
--                     ,trueBranch = 2
--                     ,falseBranch = 2
--                     ,count=0
--                     }



-- testMonkey2 = Monkey{
--                      items = [79,98]
--                     ,operation = (*19)
--                     ,divisible = 23
--                     ,trueBranch = 1
--                     ,falseBranch = 1
--                     ,count=0
--                     }

-- testMonkeys = [testMonkey0,testMonkey1,testMonkey2]
--simulate :: [Monkey] -> [Monkey]
--simulate monkeys = go monkes
simMonkey :: Int ->State [Monkey] ()
simMonkey i = do
   monkeys <- get 
   let monkey = monkeys !! i
   updateItems (items monkey)
     where
       updateItems :: [Int] -> State [Monkey] ()
       updateItems [] = return ()
       updateItems (item:rest) = do
             monkeys <- get
             let monkey = monkeys !! i
                 newMonkey = monkey{items = rest}
                 new = ((operation monkey) item ) `div` 3
                 newmonkeys = updateMonkey newMonkey monkeys
             put(newmonkeys)
             if new `mod` (divisible monkey) == 0 then put (throwItems new (trueBranch monkey) newmonkeys) >> updateItems rest
                                                  else put (throwItems new (falseBranch monkey)  newmonkeys) >> updateItems rest

       updateMonkey monkey monkeys = let (pre,target:after) = splitAt i monkeys
                                         in
                                           pre ++ (monkey:after)
                                           
       throwItems val tgt monkeys = let (pre,target:after) = splitAt tgt monkeys
                                        newitems = (items target) ++ [val]
                                        newMonkey = target {items = newitems}
                                        in
                                          pre++ (newMonkey:after)


simSingleRound :: Int -> State [Monkey] ()
simSingleRound size = go 0
  where go n = if n == size then return ()
                            else simMonkey n >> go (n+1)


simRounds :: Int -> State [Monkey] ()
simRounds n = replicateM_ n (simSingleRound 4)
