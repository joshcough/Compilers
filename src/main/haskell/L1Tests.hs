import IOHelpers
import Data.List

testDir = "./L1TestFiles"
scalaResultsDir   = testDir ++ "/scala-results"
haskellResultsDir = testDir
l1File = isSuffixOf ".L1"
sFile  = isSuffixOf ".S"
testFiles      = fmap (take 250 . filter l1File) (filesWithFullPaths testDir)
--scalaResults   = fmap (filter sFile)  (filesWithFullPaths scalaResultsDir)
--haskellResults = fmap (filter sFile)  (filesWithFullPaths haskellResultsDir)

main = do (testFiles >>= namesAndContents >>= putList)

