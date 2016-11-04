module MyGraph where
import MyArray


data Graph = Graph (Array Int [Int])
type VisitedArray = Array Int Bool


emptyGraph :: Graph
emptyGraph = Graph (array (0, (maxBound :: Int) - 1) [])


emptyVisitedArray :: VisitedArray
emptyVisitedArray = array (0, (maxBound :: Int) - 1) []


insertNode :: Graph -> Int -> [Int] -> Graph
insertNode (Graph array) node adjList = if contains array node
	then error ("Node " ++ show node ++ " already exists")
	else Graph (update node adjList array)


dfs :: [Int] -> [Int] -> Array Int [Int] -> VisitedArray -> ([Int], VisitedArray)

dfs [] result _ visitedNodes = (result, visitedNodes)

dfs (adjNode:nextNodes) result adjList visitedNodes
	| contains visitedNodes adjNode =
		dfs nextNodes result adjList visitedNodes
	
	| otherwise =
		dfs nextNodes (result ++ nextDfsResult) adjList updatedVisitedNodes
	
	where (nextDfsResult, updatedVisitedNodes) =
		dfs (adjList ! adjNode) [adjNode] adjList (update adjNode True visitedNodes)


listAvailableNodes :: Graph -> [Int]
listAvailableNodes (Graph adjList)
	| contains adjList 1 = result
	| otherwise = []
	where (result, _) = dfs (adjList ! 1) [1] adjList (update 1 True emptyVisitedArray)


parseString :: String -> Int
parseString str = case reads str :: [(Int, String)] of
	[(x, "")] -> x
	_ -> error "Cannot convert the input to ints"


lineToList :: String -> [Int]
lineToList line = map parseString (words line)


insertNodes :: [String] -> Graph -> Graph
insertNodes [] graph = graph

insertNodes (nodeString:rest) graph
	| (lineToList nodeString) == [] =
		insertNodes rest graph
	
	| otherwise =
		let (node:adjList) = lineToList nodeString
		in insertNodes rest (insertNode graph node adjList)


makeGraph :: String -> Graph
makeGraph input = insertNodes (lines input) emptyGraph
