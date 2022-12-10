
data Machine = Machine{
             x :: Int
          ,  pc :: Int
          ,  cycle :: Int
          , busy :: Bool -- during addition?
          , cursor :: Int
          , output :: String
          , program :: [Instruction]
          } deriving (Show)


data Instruction = AddX Int | Nop deriving (Show)


fromString :: String -> Instruction
fromString xs =let cmd =  words xs in
                 case head cmd of
                   "noop" -> Nop
                   "addx" -> AddX (read $ cmd!!1)

initMachine :: Machine
initMachine = Machine 1 0 0 False 0 [] []

interrept :: String -> [Instruction]
interrept xs = map fromString $ lines xs

loadProgram :: Machine -> [Instruction] -> Machine
loadProgram machine instrs = machine {program = instrs}

step :: Machine -> Machine
step (Machine x pc cycle busy cur out  program) | busy == True = let (AddX op) = program !! pc
                                                                     char = if abs(x - cur) > 1 then '.' else '#'
                                                                     cur' = if cur == 39 then 0 else cur +1
                                                                     out'  = char:out
                                                                 in
                                          Machine (x+ op) (pc+1) (cycle+1) False cur' out'  program

                                       | otherwise = let cmd = program !! pc
                                                         char = if abs(x - cur) > 1 then '.' else '#'
                                                         cur' = if cur == 39 then 0 else cur +1
                                                         out'  = char:out

                                                     in
                                                       case cmd of
                                                         Nop -> Machine x (pc+1) (cycle+1) False cur' out' program
                                                         AddX op -> Machine x pc (cycle+1) True cur' out' program
                                              
stepOver :: Int -> Machine -> Machine
stepOver 0 machine = machine
stepOver n machine = stepOver (n-1) (step machine)


part1_item :: Int -> Machine -> Int
part1_item n machine = let (Machine x pc cycle busy cur out program) = stepOver n machine in
                         (n+1) * x
                           

part2_item :: Int -> Machine -> String
part2_item n machine = let (Machine x pc cycle busy cur out program) = stepOver n machine
                       in
                            reverse out


linesAt :: Int -> String -> [String]
linesAt n [] = []
linesAt n xs = let (a,as) = splitAt n xs in
                 if length xs < n then [xs]
                                  else
                                   a: (linesAt n as)


part2 :: IO ()
part2 = do
  file <- readFile "input.txt"
  let programs = interrept file
  let machine = loadProgram initMachine programs
  let result = part2_item 240 machine
  mapM_ putStrLn (linesAt 40 result)
