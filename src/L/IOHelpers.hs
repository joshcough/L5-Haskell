module L.IOHelpers
  (
    changeDir
   ,changeExt
   ,fileArgMain
   ,getExtension
   ,getDir
   ,getFileName
   ,getRecursiveContents
   ,getRecursiveContentsByExt
   ,listFiles
   ,mapFileContents
   ,mapFileContentsAndPrint
   ,putFileNames
   ,putList
   ,readMapAndWriteFileArg
   ,readMapAndWriteFileArgAndPath
   ,touch
   ,withFileArg
   ,withFileArgT
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

dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse

dropRightWhile :: (a -> Bool) -> [a] -> [a]
dropRightWhile f = reverse . dropWhile f . reverse

notDot = (/=) '.'

takeRightWhile :: (a -> Bool) -> [a] -> [a]
takeRightWhile f = reverse . takeWhile f . reverse

getExtension, getFileName :: FilePath -> String
getExtension = takeRightWhile notDot
getFileName = takeRightWhile ('/' /=)

getDir :: FilePath -> FilePath
getDir file = if null d then "." else d where
  d = dropRight (length $ getFileName file) file

changeDir :: FilePath -> FilePath -> FilePath
changeDir file newDir = newDir ++ "/" ++ getFileName file

changeExt :: FilePath -> String -> String
changeExt file newExt = (dropRightWhile notDot file) ++ newExt

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

mapFileContentsAndPrint :: Show a => (String -> a) -> FilePath -> IO ()
mapFileContentsAndPrint f file = mapFileContents f file >>= putStrLn . show

withFileArg :: (FilePath -> IO a) -> IO a
withFileArg f = fmap (!! 0) getArgs >>= f

fileArgMain :: Show a => (String -> a) -> IO ()
fileArgMain f =  withFileArg $ mapFileContentsAndPrint f 

-- read input file from first command line arg, 
-- map its name and contents over f, 
-- and return the path and the results 
withFileArgT :: (FilePath -> String -> a) -> IO (FilePath, a)
withFileArgT f = do           
  inputFile <- fmap (!! 0) getArgs
  a <- mapFileContents (f inputFile) inputFile
  return (inputFile, a)

-- read the given input file, and compile it
mapFileContents :: (String -> a) -> FilePath -> IO a
mapFileContents f inputFile =
  do s <- readFile inputFile
     length s `seq` return (f s)

-- read the first command line argument
-- its a file - read that file
-- run the given function on its contents
-- write the results to a new file with the same name as the original file
-- but with a different extension (the given one)
readMapAndWriteFileArg :: (String -> String) -> String -> IO ()
readMapAndWriteFileArg f newExt = readMapAndWriteFileArgAndPath (\_ -> f) newExt

readMapAndWriteFileArgAndPath :: (FilePath -> String -> String) -> String -> IO ()
readMapAndWriteFileArgAndPath f newExt = do
  (file, s) <- withFileArgT f
  writeFile (changeExt file newExt) s
