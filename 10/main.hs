
data Machine = Machine{
             x :: Int
          ,  pc :: Int
          ,  cycle :: Int
          , busy :: Bool -- during addition?
          , program :: [Instruction]
          } deriving (Show)


data Instruction = AddX Int | Nop deriving (Show)


fromString :: String -> Instruction
fromString xs =let cmd =  words xs in
                 case head cmd of
                   "noop" -> Nop
                   "addx" -> AddX (read $ cmd!!1)

initMachine :: Machine
initMachine = Machine 1 0 0 False []

interrept :: String -> [Instruction]
interrept xs = map fromString $ lines xs

loadProgram :: Machine -> [Instruction] -> Machine
loadProgram machine instrs = machine {program = instrs}

step :: Machine -> Machine
step (Machine x pc cycle busy program) | busy == True = let (AddX op) = program !! pc in
                                          Machine (x+ op) (pc+1) (cycle+1) False program

                                       | otherwise = let cmd = program !! pc in
                                                       case cmd of
                                                         Nop -> Machine x (pc+1) (cycle+1) False program
                                                         AddX op -> Machine x pc (cycle+1) True program
                                              
stepOver :: Int -> Machine -> Machine
stepOver 0 machine = machine
stepOver n machine = stepOver (n-1) (step machine)


part1_item :: Int -> Machine -> (Int,Int,Int,Bool,Instruction)
part1_item n machine = let (Machine x pc cycle busy program) = stepOver n machine in
                         ((n+1) * x,pc,cycle,busy,(program !! pc))
                           

part1 :: IO ()
part1 = do
  file <- readFile "sample.txt"
  let programs = interrept file
  let machine = loadProgram initMachine programs
  let signal_inspector = [20,60,100,140,180,220]
  let result = map (\n -> part1_item (n-1) machine) signal_inspector
  putStrLn $ show $ result
