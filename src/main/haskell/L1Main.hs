import System.Environment 
import System.IO
import Data.List
import Read
import L1AST
import L1Parser
import L1X86

dropRightWhile f = reverse . dropWhile f . reverse
changeExtension file newExt = (dropRightWhile (\c -> not (c == '.')) file) ++ newExt
compile = genCode . parse . sread

main = do  
   fileNames <- getArgs
   -- just read the first file here.
   -- i suppose later on we could compile many files...
   let inputFile = fileNames !! 0
   let outputFile = changeExtension inputFile "S"
   results <- fmap compile (readFile inputFile)
   writeFile outputFile results

