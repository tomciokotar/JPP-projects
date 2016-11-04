import System.Environment
import MyGraph

main = do
	args <- getArgs
	input <- case args of
		(file: _) -> readFile file
		[] -> getContents
	print (listAvailableNodes (makeGraph input))
