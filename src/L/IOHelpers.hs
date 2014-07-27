module L.IOHelpers
  (
    changeDir
   ,changeExtension
   ,fileArgMain
   ,getExtension
   ,getRecursiveContents
   ,getRecursiveContentsByExt
   ,listFiles
   ,mapFileContents
   ,mapFileContentsAndPrint
   ,putFileNames
   ,putList
   ,touch
   ,withFileArg
  ) where

import Control.Applicative
import Control.Monad (forM)
import Data.List
import Data.Traversable hiding (forM)
import Data.Foldable hiding (concat, notElem)
import L.Utils (endsWith)
import System.Environment 
import System.FilePath ((</>))
import System.IO
import System.Directory

(|>) = flip (.)
($$>) = flip fmap
listFiles = getDirectoryContents

-- put a whole list of showables
putList :: Show a => [a] -> IO ()
putList xs = sequenceA_ (map (putStrLn . show) xs)

-- put the given a, and then a new line
putWithNewLine :: Show a => a -> IO ()
putWithNewLine a = putStrLn (show a) >> putStrLn "\n"

-- put each a in the given list, and a new line after each too.
putWithNewLineList :: Show a => [a] -> IO ()
putWithNewLineList xs = sequenceA_ (map putWithNewLine xs)

touch :: FilePath -> IO ()
touch f = writeFile f ""

-- full pathnames for every file in the given directory
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
fileListEqual fs1 fs2 = (sort fs1 == sort fs2 &&) <$> contentsEqual where
  contentsEqual :: IO Bool
  contentsEqual =
    Data.List.foldl (\b (f1, f2) -> (&&) <$> b <*> (filesEqual f1 f2)) (return True) (zip fs1 fs2)
  -- zip :: [a] -> [b] -> [(a, b)]
  -- foldl :: (a -> b -> a) -> a -> [b] -> a
--  do d1Contents <- contents fs1
--     d2Contents <- contents fs2
--     return $ sort fs1 == sort fs2 && d1Contents == d2Contents

-- call filesEqual for all the files in the given directories
dirsEqual :: FilePath -> FilePath -> IO Bool
dirsEqual d1 d2 =
  do d1Filenames <- filesWithFullPaths d1
     d2Filenames <- filesWithFullPaths d2
     fileListEqual d1Filenames d2Filenames

-- read the contents of a file
-- transform the contents
-- write them back out to a new file
transformFile :: FilePath -> (String -> String) -> FilePath -> IO ()
transformFile inFile f outFile = f <$> (readFile inFile) >>= (writeFile outFile)

{------------------------
Be careful, as these ones are pretty dangerous, it seems.
I was running into "too many open file errors".
------------------------}

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

dropRightWhile :: (a -> Bool) -> [a] -> [a]
dropRightWhile f = reverse . dropWhile f . reverse

notDot = (/=) '.'

takeRightWhile :: (a -> Bool) -> [a] -> [a]
takeRightWhile f = reverse . takeWhile f . reverse

getExtension, getFileName :: FilePath -> String
getExtension = takeRightWhile notDot
getFileName = takeRightWhile ('/' /=)

changeDir :: FilePath -> FilePath -> FilePath
changeDir file newDir = newDir ++ "/" ++ getFileName file

changeExtension :: FilePath -> String -> String
changeExtension file newExt = (dropRightWhile notDot file) ++ newExt

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then getRecursiveContents path else return [path]
  return (concat paths)

getRecursiveContentsByExt :: FilePath -> String -> IO [FilePath]
getRecursiveContentsByExt dir ext = 
  getRecursiveContents dir >>= (return . filter (endsWith ext))

mapFileContents :: (String -> a) -> FilePath -> IO a
mapFileContents f file = fmap f $ readFile file

mapFileContentsAndPrint :: Show a => (String -> a) -> FilePath -> IO ()
mapFileContentsAndPrint f file = mapFileContents f file >>= putStrLn . show

withFileArg :: (FilePath -> IO ()) -> IO ()
withFileArg f = fmap (!! 0) getArgs >>= f

fileArgMain :: Show a => (String -> a) -> IO ()
fileArgMain f =  withFileArg $ mapFileContentsAndPrint f 
