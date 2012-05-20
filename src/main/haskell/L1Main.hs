import System.Environment 
import System.IO
import Data.List
import Read
import L1AST
import L1Parser
import L1X86

compile = genCode . parse . sread

main = do  
   fileNames <- getArgs
   -- just read the first file here.
   -- i suppose later on we could compile many files...
   contents <- readFile $ fileNames !! 0
   putStrLn $ compile contents

