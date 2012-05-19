module IOHelpers where

import System.Environment 
import System.IO
import System.Directory
import TestHelpers

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

