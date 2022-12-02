
-- FIXME: there should be a clever way of making these types as instance of Bounded 
data RPS = Rock | Paper | Scissors deriving (Show,Eq)

data Result = Win | Draw | Lose deriving Show

-- RPS is not an Ord because it does not satisfy transitivity, but we can still use `compare` to simpfy implementation

instance Ord RPS where
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Paper Rock = GT
  compare Scissors Paper = GT
  compare Rock Scissors = GT
  compare a b | a == b = EQ


charToResult :: Char -> Result
charToResult char = case lookup char zs of
                      Just b -> b
                      _ -> error "wrong characters, pelase only use X Y Z"
                    where zs = zip "XYZ" [Lose,Draw,Win]

--FIXME: ugly implementation
reaction :: RPS -> Result -> RPS
reaction a Draw = a
reaction a Win =head $ filter (\c -> a < c) [Rock,Paper,Scissors]
reaction a Lose =head $filter (\c -> a > c) [Rock,Paper,Scissors]

charToRPS :: Char -> RPS
charToRPS  char = case lookup char zs of
                      Just b -> b
                      _ -> error "wrong characters, should only use A B C and X Y Z"
                    where zs = zip "ABC" [Rock,Paper,Scissors] ++  zip "XYZ" [Rock,Paper,Scissors]

shapeScores :: RPS -> Int
shapeScores Rock = 1
shapeScores Paper = 2
shapeScores Scissors = 3

resultScores :: Result -> Int
resultScores Lose = 0
resultScores Draw = 3
resultScores Win  = 6

solve :: RPS -> RPS -> Result
solve a b | a > b = Lose
          | a == b = Draw
          | a < b = Win

prepare_part1 :: String -> [(RPS,RPS)]
prepare_part1 str =map (\xs -> let xs' = map charToRPS xs in (xs'!!0, xs'!!1)) $  map (map head) $ map words $ lines str
-- Careful : map (charToRPS zs) . head $ words "A Y" is different from
--        map (charToRPS zs) $ map head $ words "A Y"

prepare_part2 :: String -> [(RPS,Result)]
prepare_part2 str =map (\(a:b:_) -> (charToRPS a, charToResult b)) $  map (map head) $ map words $ lines str


part1 :: [(RPS,RPS)] -> Int
part1 input = sum result
         where result = map (\(a,b) -> shapeScores b + resultScores  (solve a b)) input

part2 :: [(RPS,Result)] -> Int
part2 input = sum result
         where result = map (\(a,b) -> resultScores b + shapeScores  (reaction a b)) input


main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn $ show $ part1 $ prepare_part1 $ file
  putStrLn $ show $ part2 $ prepare_part2 $ file
