module IOHelpers where

import System.Environment 
import System.IO
import System.Directory
import TestHelpers

import Data.List
import Data.Traversable
import Data.Foldable
import Control.Applicative

(|>) = flip (.)
($$>) = flip fmap

putStringAndNL :: String -> IO ()
putStringAndNL s = putStrLn s >> putStrLn "\n"

putStringsAndNLs :: [String] -> IO ()
putStringsAndNLs xs = sequenceA_ (map putStringAndNL xs)

putList :: Show a => [a] -> IO ()
putList xs = sequenceA_ (map (putStrLn . show) xs)

-- full pathnames for every file in the given directory
filesWithFullPaths :: FilePath -> IO [FilePath]
filesWithFullPaths dir = 
  drop 2 <$> getDirectoryContents dir $$> (map (\f -> dir ++ "/" ++ f))

-- contents for a list of files
contents :: [FilePath] -> IO [String]
contents = fmap readFile |> Data.Traversable.sequence

-- contents for every file in a directory
dirContents :: FilePath -> IO [String]
dirContents dir = filesWithFullPaths dir >>= contents

-- filenames and contents for every file in a directory
dirFileNamesAndContents :: FilePath -> IO [(String, String)]
dirFileNamesAndContents dir = zip <$> (filesWithFullPaths dir) <*> (dirContents dir)

-- filenames and contents of every file in the given list
namesAndContents :: [FilePath] -> IO [(String, String)]
namesAndContents fileNames = zip fileNames <$> contents fileNames

--sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
putFileNames :: FilePath -> IO ()
putFileNames dir = filesWithFullPaths dir >>= putList 

-- checks to see if the contents of the given files are equal
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual f1 f2 = (==) <$> (readFile f1) <*> (readFile f2)

fileListEqual :: [FilePath] -> [FilePath] -> IO Bool
fileListEqual fs1 fs2 =
  do d1Contents <- contents fs1
     d2Contents <- contents fs2
     return $ sort fs1 == sort fs2 && d1Contents == d2Contents

-- checks to see if the two given directories
--   1) contain all the same files
--   2) the contents of the files are equal
dirsEqual :: FilePath -> FilePath -> IO Bool
dirsEqual d1 d2 =
  do d1Filenames <- filesWithFullPaths d1
     d2Filenames <- filesWithFullPaths d2
     fileListEqual d1Filenames d2Filenames

readDoWrite :: FilePath -> (String -> String) -> FilePath -> IO ()
readDoWrite inFile f outFile = f <$> (readFile inFile) >>= (writeFile outFile)
