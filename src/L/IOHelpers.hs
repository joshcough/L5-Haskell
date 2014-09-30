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
import System.Directory

($$>) :: IO a -> (a -> b) -> IO b
($$>) = flip fmap

listFiles :: FilePath -> IO [FilePath]
listFiles = getDirectoryContents

-- put a whole list of showables
putList :: Show a => [a] -> IO ()
putList xs = sequenceA_ (map (putStrLn . show) xs)

touch :: FilePath -> IO ()
touch f = writeFile f ""

-- full pathnames for every file in the given directory
filesWithFullPaths :: FilePath -> IO [FilePath]
filesWithFullPaths dir = drop 2 <$> listFiles dir $$> (map (\f -> dir ++ "/" ++ f))

--sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
putFileNames :: FilePath -> IO ()
putFileNames dir = filesWithFullPaths dir >>= putList 

-- TODO: move these to L.Utils
dropRight :: Int -> [a] -> [a]
dropRight n = reverse . drop n . reverse

dropRightWhile :: (a -> Bool) -> [a] -> [a]
dropRightWhile f = reverse . dropWhile f . reverse

notDot :: Char -> Bool
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
