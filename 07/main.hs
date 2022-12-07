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
                           
                             
-- -- part1:: FDZipper -> [String,Int]
-- -- part1 (root,_) = go root []
-- --                where go node result = case node of
-- --                                         (FDDirectory name contents) ->
-- --                                           let 
-- --                                           map getSize files

-- --findCandidateSize :: [Entry] -> Int
-- --findCandidateSize entries =


  
