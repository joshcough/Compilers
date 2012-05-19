import IOHelpers
import Data.List

testDir = "../../test/compilers/test-fest/1-test/cough/1-test"
l1File = isSuffixOf ".L1"
testFiles = fmap (filter l1File) (filesWithFullPaths testDir)

main = do (testFiles >>= namesAndContents >>= putList)

