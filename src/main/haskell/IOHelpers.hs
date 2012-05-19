module IOHelpers where

import System.Environment 
import System.IO
import System.Directory
import TestHelpers

import Data.List
import Data.Traversable
import Data.Foldable
import Control.Applicative

putStringAndNL :: String -> IO ()
putStringAndNL s = putStrLn s >> putStrLn "\n"

putStringsAndNLs :: [String] -> IO ()
putStringsAndNLs xs = sequenceA_ (fmap putStringAndNL xs)

putList :: Show a => [a] -> IO ()
putList xs = sequenceA_ (fmap (\a -> putStrLn (show a)) xs)

-- full pathnames for every file in the given directory
filesWithFullPaths :: FilePath -> IO [FilePath]
filesWithFullPaths dir = 
  fmap (fmap (\f -> dir ++ "/" ++ f)) (fmap (drop 2) $ getDirectoryContents dir)

-- contents for a list of files
contents :: [FilePath] -> IO [String]
contents = (Data.Traversable.sequence . fmap readFile)

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

-- checks to see if the two given directories
--   1) contain all the same files
--   2) the contents of the files are equal
dirsEqual :: FilePath -> FilePath -> IO Bool
dirsEqual d1 d2 = 
  do d1Filenames <- fmap sort $ filesWithFullPaths d1
     d2Filenames <- fmap sort $ filesWithFullPaths d2
     d1Contents  <- contents d1Filenames
     d2Contents  <- contents d2Filenames
     return $ d1Filenames == d2Filenames && d1Contents == d2Contents
     
