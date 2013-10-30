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

--main = do (testFiles >>= namesAndContents >>= putList)

main = error "redo"

{--
filesWithFullPaths :: FilePath -> IO [FilePath]
filesWithFullPaths dir = drop 2 <$> listFiles dir $$> (map (\f -> dir ++ "/" ++ f))

--sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
putFileNames :: FilePath -> IO ()
putFileNames dir = filesWithFullPaths dir >>= putList

-- checks to see if the contents of the given files are equal
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual f1 f2 = (==) <$> (readFile f1) <*> (readFile f2)

-- checks to see if the two given lists
--  1) contain all the same filenames
--  2) the contents of the files are equal
fileListEqual :: [FilePath] -> [FilePath] -> IO Bool
--}