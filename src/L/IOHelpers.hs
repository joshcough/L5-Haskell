module L.IOHelpers
  (
    changeDir
   ,changeExt
   ,getExtension
   ,getFileName
   ,mapFileContents
   ,putList
   ,touch
   ,withFileArg
  ) where

import Control.Applicative
import Data.Foldable hiding (concat, notElem)
import System.Environment 
import System.Directory

($$>) :: IO a -> (a -> b) -> IO b
($$>) = flip fmap

-- put a whole list of showables
putList :: Show a => [a] -> IO ()
putList xs = sequenceA_ (map (putStrLn . show) xs)

touch :: FilePath -> IO ()
touch f = writeFile f ""

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

changeDir :: FilePath -> FilePath -> FilePath
changeDir file newDir = newDir ++ "/" ++ getFileName file

changeExt :: FilePath -> String -> String
changeExt file newExt = (dropRightWhile notDot file) ++ newExt

withFileArg :: (FilePath -> IO a) -> IO a
withFileArg f = fmap (!! 0) getArgs >>= f

-- read the given input file, and compile it
mapFileContents :: (String -> a) -> FilePath -> IO a
mapFileContents f inputFile =
  do s <- readFile inputFile
     length s `seq` return (f s)
