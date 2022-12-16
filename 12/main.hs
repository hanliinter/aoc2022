import Data.Char
import Data.List
--import Debug.Trace
import qualified Data.Map as M
  
type Pos   = (Int,Int)
type Graph = [[Node]]
type Node  = (Pos,Char)
data Queue a = Queue [a] [a]

instance (Show a)=> Show (Queue a) where
  show (Queue xs ys) = "Queue " ++ show (xs ++reverse ys)


enqueue :: Queue a -> a -> Queue a
enqueue (Queue xs ys) a = (Queue xs (a:ys))

enqueues :: Queue a -> [a] -> Queue a
enqueues q = foldr (flip enqueue) q


dequeue :: Queue a -> (Queue a,a)
dequeue (Queue (x:xs) ys) = ((Queue xs ys) ,x)
dequeue (Queue [] ys)   = let (x:xs) = reverse ys in
                      ((Queue xs []),x)



isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

initQueue :: Queue a
initQueue = Queue [] []

singleton :: a -> Queue a
singleton a = Queue [] [a]


markPos :: String -> Graph
markPos xs = let xs' = map (zip [0..]) $ lines xs
                 zs  = zipWith (\i js -> map (\(j,v) -> ((i,j),v) ) js ) [1..] xs'
             in
               zs

findPos :: Char -> Graph -> Node
findPos val =  head .filter (\(p,v) -> v == val) .concat 

sourcePos = findPos 'S'
targetPos = findPos 'E'

findNode :: Pos -> Graph -> Maybe Node
findNode pos g = case filter (\(p,v) -> p == pos) $ concat g of
                   [] -> Nothing
                   (a:_) -> Just a

getNode :: Pos -> Graph -> Node
getNode pos g = head $ filter (\(p,v) -> p == pos) $ concat g

bfs :: Graph ->  Pos -> [Pos] -> Queue Node-> M.Map Pos Pos -> M.Map Pos Pos 
bfs g t visited queue parent | isEmpty ( (queue)) = parent
                             | otherwise = let (newQueue,v) = dequeue queue
                                               pos = fst v
                                               newNodes = expandNode g v visited
                                               newNodesPos = expandNodePos g v visited
                                           in
                                             if (pos) == t then  parent
                                                           else case  (fst v) `elem` visited of
                                                                  True ->  bfs g t visited (newQueue) parent
                                                                  False -> bfs g t (pos:visited) (enqueues newQueue newNodes) (insertEdgeTo  (pos) newNodesPos parent)

insertEdgeTo :: Pos -> [Pos] -> M.Map Pos Pos -> M.Map Pos Pos
insertEdgeTo p ns parent = foldr (\n parent ->M.insert ( n) p parent )  parent ns


expandNodePos :: Graph -> Node -> [Pos] -> [Pos]
expandNodePos g ((i,j),v) visited = let pos = [(i,j+1),(i,j-1),(i+1,j),(i-1,j)]
                                        validPos = filter (\pos -> (isValidCandidate g pos v) && (not $ pos `elem` visited)) pos
                                    in
                                       validPos

expandNode :: Graph -> Node ->[Pos]-> [Node]
expandNode g ((i,j),v) visited = let pos = [(i,j+1),(i,j-1),(i+1,j),(i-1,j)]
                                     validPos = filter (\pos -> isValidCandidate g pos v && (not $ pos `elem` visited)) pos
                                     nodes = map (\pos -> getNode pos g) validPos
                                 in
                                 nodes

isValidCandidate :: Graph -> Pos -> Char -> Bool
isValidCandidate g pos c = case findNode pos g of
                             Nothing -> False
                             Just (p,v) -> let v' = if v == 'E' then 'z' else v
                                           in    
                                             if (ord v') - (ord c) <= 1 then True
                                                                        else False





bfs2 :: Graph ->  Char -> [Pos] -> Queue Node-> M.Map Pos Pos -> (M.Map Pos Pos, Maybe Node)
bfs2 g t visited queue parent | isEmpty ( (queue)) = (parent,Nothing)
                              | otherwise = let (newQueue,v) = dequeue queue
                                                curr = snd v
                                                pos  = fst v
                                                newNodes = expandNode2 g v visited
                                                newNodesPos = expandNodePos2 g v visited
                                           in
                                             if curr == t then  (parent,Just v)
                                                           else case  (fst v) `elem` visited of
                                                                  True ->  bfs2 g t visited (newQueue) parent
                                                                  False -> bfs2 g t (pos:visited) (enqueues newQueue newNodes) (insertEdgeTo  (pos) newNodesPos parent)



expandNodePos2 :: Graph -> Node -> [Pos] -> [Pos]
expandNodePos2 g ((i,j),v) visited = let pos = [(i,j+1),(i,j-1),(i+1,j),(i-1,j)]
                                         validPos = filter (\pos -> (isValidCandidate2 g pos v) && (not $ pos `elem` visited)) pos
                                    in
                                       validPos

expandNode2 :: Graph -> Node ->[Pos]-> [Node]
expandNode2 g ((i,j),v) visited = let pos = [(i,j+1),(i,j-1),(i+1,j),(i-1,j)]
                                      validPos = filter (\pos -> isValidCandidate2 g pos v && (not $ pos `elem` visited)) pos
                                      nodes = map (\pos -> getNode pos g) validPos
                                 in
                                 nodes

isValidCandidate2 :: Graph -> Pos -> Char -> Bool
isValidCandidate2 g pos c = case findNode pos g of
                             Nothing -> False
                             Just (p,v) -> let v' = if v == 'S' then 'a' else v
                                           in    
                                             if (ord c) - (ord v') <= 1 then True
                                                                        else False





getPath :: Pos -> M.Map Pos Pos -> [Pos]
getPath p parent = go p parent [p]
  where go p parent result = case M.lookup p parent of
                               Just a -> go a parent (a:result)
                               Nothing -> result

part1 :: FilePath -> IO ()
part1 filename = do
  content <- readFile filename
  let g = markPos content
      sPos =  fst (sourcePos g)
      tPos  =  fst  (targetPos g)
      queue = singleton (sPos,'a')
      parent = M.empty
      parent' =  bfs g tPos []  queue parent
      fullPath = getPath tPos ( parent')
      answer = length (fullPath \\ [sPos])
  --putStrLn $ "sNode" ++ show sPos ++ "," ++ "target at:" ++ show tPos     
  putStrLn $ show answer
--  return parent'
  
part2 :: FilePath -> IO ()
part2 filename = do
  content <- readFile filename
  let g = markPos content
      sPos =  fst (targetPos g)
      queue = singleton (sPos,'z')
      parent = M.empty
      (parent',t) =  bfs2 g 'a' []  queue parent
  case t of
        Nothing -> error "not found"
        Just tPos ->
          let fullPath = getPath (fst tPos) ( parent')
              answer = length (fullPath \\ [sPos])
          in    
  --putStrLn $ "sNode" ++ show sPos ++ "," ++ "target at:" ++ show tPos     
              putStrLn $ show answer
--  return parent'


allCandidateStartPoints :: Graph -> [Node]
allCandidateStartPoints = filter (\(p,v) -> v == 'S' || v == 'a') .concat

shortestPath :: Pos -> Pos -> Graph -> Int
shortestPath sPos tPos g = let queue = singleton (sPos,'a')
                               parent = M.empty
                               parent' = bfs g tPos [] queue parent
                               fullPath = getPath tPos parent'
                               answer = length (fullPath \\ [sPos])
                          in
                           answer


