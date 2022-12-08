import Data.List
import Debug.Trace

data CommandDetail = Cd String | Ls deriving (Show)
data Entry = Command CommandDetail | Directory String | File String Int deriving (Show)

parseLine :: String -> Entry
parseLine cmd@('$':_) = let cmdList = words cmd in
                          case cmdList !! 1 of
                            "cd" -> Command $ Cd (cmdList !! 2)
                            "ls" -> Command $ Ls
                            _    -> error "unexpected error in parsing"

parseLine directory = let par = take 3 directory in
                        if par == "dir" then Directory $ (words directory) !! 1
                                        else let (size:filename:_) = words directory in
                                               File filename (read size)

parseEntry :: [String] -> [Entry]
parseEntry xs = map parseLine xs

-- it is not necessary to generate a tree for this puzzle. But I like it, it is also an opptunity for practising zipper
data FileDirectory = FDFile String Int | FDDirectory String [FileDirectory] [FileDirectory] deriving (Show)
data FDCrumb = FDCrumb String [FileDirectory] [FileDirectory] deriving (Show)
type FDZipper = (FileDirectory,[FDCrumb]) 

fdUp :: FDZipper -> FDZipper
fdUp (root, []) = (root,[])
fdUp (fd, FDCrumb name folders files :bs) = (FDDirectory name (fd:folders) files, bs)

gotoFolder :: String -> FDZipper -> FDZipper
gotoFolder targetName (FDDirectory parentName subFolders files, bs) =
      case  break (nameIs targetName) subFolders of
        (ls, item:rs) -> (item, FDCrumb parentName (ls ++ rs) files:bs )
        (ls,[])       -> error ("items =" ++show subFolders )
topMost :: FDZipper -> FDZipper
topMost (t,[]) = (t,[])
topMost z = topMost (fdUp z)

nameIs :: String -> FileDirectory -> Bool
nameIs name (FDDirectory fdName _ _ ) = name == fdName
nameIs name (FDFile fName _) = name == fName
--data zipper = (FileDirectory)

buildTree :: [Entry] -> FDZipper
buildTree = foldl step root 

changeDirectory :: FDZipper -> String -> FDZipper
changeDirectory fdZipper dir | dir == "/" = topMost fdZipper
                             | dir == ".." = fdUp fdZipper
                             | otherwise = gotoFolder dir  fdZipper
      
makeDirectory :: FDZipper -> String -> FDZipper
makeDirectory (FDDirectory fdName subFolders files, bs) newDirName =
                                       let newDirectory = FDDirectory newDirName [] [] in
                                       (FDDirectory fdName (newDirectory:subFolders) files , bs)

createFile :: FDZipper -> String ->Int -> FDZipper
createFile (FDDirectory fdName subFolders files,bs) newFileName size = let newFile = FDFile newFileName size in
                                                                (FDDirectory fdName subFolders (newFile:files), bs)

-- -- changeDirectory zipper(fd, FDCrumb parentName)
root = (FDDirectory "/" [] [],[])

step :: FDZipper -> Entry -> FDZipper
step fd (Command (Cd dir)) = changeDirectory fd dir
step fd (Command Ls)     = fd
step fd (Directory dName) = makeDirectory fd dName
step fd (File name size)  = createFile fd name size
                           


-- rose tree?

calcSize:: FileDirectory -> [(String,Int)]
calcSize root = go root [] "ROOT"
         where
           go (FDDirectory name [] files) result parentName = (parentName ++ "/" ++name,totalFileSize files):result
           go (FDDirectory name subDir files ) result parentName  =  (parentName ++ "/" ++name,totalFileSize files + subDirSize):subResult  ++ result 
            where subResult  = concatMap (\x -> go x [] (parentName ++ "/" ++ name)) subDir
                  subDirNames :: [String]
                  subDirNames = map (\(FDDirectory subName _ _) -> parentName ++ "/" ++  name ++"/" ++ subName ) subDir
                  
                  subDirSize  = sum $ map snd  $ filter (\(x,_) -> x `elem` (subDirNames)) (subResult)
                   


totalFileSize :: [FileDirectory] -> Int
totalFileSize files = sum $ map getFileSize files
    where getFileSize (FDFile _ size) = size
-- --findCandidateSize :: [Entry] -> Int
-- --findCandidateSize entries =

totalFileSizeFromLines :: [Entry] -> Int
totalFileSizeFromLines xs = sum $ map getFileSize xs
  where getFileSize (File _ size) = size
        getFileSize _ = 0



calc ::String -> IO ()
calc str = do
   file <- readFile str
   let (treeRoot,_) = topMost $ buildTree $ parseEntry $ lines file
       totalFileSize = totalFileSizeFromLines $ parseEntry $ lines file
       result = calcSize treeRoot
       part1_answer = sum $ filter (<= 100000) $ map snd result
       outMost = case lookup "/" result of
                   Just n -> n
                   _ -> error "no root found!"
       freeSpace = 70000000 - totalFileSize
       threshold = 30000000 - freeSpace
       part2_answer = minimum $ filter (>threshold) $ sort $ map snd result
   putStrLn $ show totalFileSize
   --putStrLn $ show treeRoot
   putStrLn $  (show part2_answer )
   return ()
