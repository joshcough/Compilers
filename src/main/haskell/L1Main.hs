import System.Environment 
import System.IO
import Data.List

main = do  
   fileNames <- getArgs
   -- just read the first file here.
   -- i suppose later on we could compile many files...
   contents <- readFile $ fileNames !! 0
   putStrLn contents
