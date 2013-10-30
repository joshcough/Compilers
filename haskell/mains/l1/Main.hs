module Main (main) where

import Data.List
import Data.Traversable
import L.Read
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.L1.L1 (compileL1)
import System.Environment 
import System.IO

-- just read the first file here. i suppose later on i could compile many files...
main = fmap (!! 0) getArgs >>= compileToFile where
  compileToFile inputFile =
    let outputFile = changeExtension inputFile "S" in
    fmap ((either error id) . compileL1) (readFile inputFile) >>= writeFile outputFile
