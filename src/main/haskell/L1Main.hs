import System.Environment 
import System.IO
import Data.List
import Read
import L1AST
import L1Parser

main = do  
   fileNames <- getArgs
   -- just read the first file here.
   -- i suppose later on we could compile many files...
   contents <- readFile $ fileNames !! 0
   putStrLn contents
   putStrLn $ show $ sread contents
   putStrLn $ show $ parse $ sread contents
