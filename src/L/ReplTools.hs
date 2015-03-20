module L.ReplTools (
   cat
  ,ls
  ,quickCompile
  ,quickCompileL1
  ,quickCompileL2
  ,quickCompileL3
  ,quickCompileL4
  ,quickCompileL5
  ,quickCompileString
  ,quickCompileTurtles
  ,quickInterp
  ,quickRunNative
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Default
import L.Compiler
import L.L1.L1
import L.L2.L2
import L.L3.L3
import L.L4.L4
import L.L5.L5
import System.Directory
import System.FilePath.Lens

opts :: CompilationOptions
opts = def

quickCompileString :: Show o => Language i o -> String -> String
quickCompileString l s = show . runVal $ compileString l s opts s

quickCompileL1 :: String -> String
quickCompileL1 = quickCompileString l1Language

quickCompileL2 :: String -> String
quickCompileL2 = quickCompileString l2Language

quickCompileL3 :: String -> String
quickCompileL3 = quickCompileString l3Language

quickCompileL4 :: String -> String
quickCompileL4 = quickCompileString l4Language

quickCompileL5 :: String -> String
quickCompileL5 = quickCompileString l5Language

-- TODO: really need a pretty printer for displaying the result in the repl
quickCompile :: FilePath -> IO ()
quickCompile inputFile = g $ inputFile^.extension  where
  f :: Show o => Language i o -> IO ()
  f l = liftM show (compileFileAndWriteResult l opts inputFile) >>= putStrLn
  g ".L1" = f l1Language
  g ".L2" = f l2Language
  g ".L3" = f l3Language
  g ".L4" = f l4Language
  g ".L5" = f l5Language
  g _     = error $ "Error: bad L file: " ++ inputFile

quickCompileTurtles :: FilePath -> IO ()
quickCompileTurtles inputFile = g $ inputFile^.extension where
  f :: Language i o -> IO ()
  f l = compileTurtlesFile l opts inputFile
  g ".L1" = f l1Language
  g ".L2" = f l2Language
  g ".L3" = f l3Language
  g ".L4" = f l4Language
  g ".L5" = f l5Language
  g _     = error $ "Error: bad L file: " ++ inputFile

quickRunNative :: FilePath -> IO ()
quickRunNative inputFile = g $ inputFile^.extension  where
  f :: Language i o -> IO ()
  f l = liftM show (compileFileAndRunNative l opts inputFile) >>= putStrLn
  g ".L1" = f l1Language
  g ".L2" = f l2Language
  g ".L3" = f l3Language
  g ".L4" = f l4Language
  g ".L5" = f l5Language
  g _     = error $ "Error: bad L file: " ++ inputFile

quickInterp :: FilePath -> IO ()
quickInterp inputFile = g $ inputFile^.extension where
  f :: Language i o -> IO ()
  f l = either id id <$> interpretFile l inputFile >>= putStrLn
  g ".L1" = f l1Language
  g ".L2" = f l2Language
  g ".L3" = f l3Language
  g ".L4" = f l4Language
  g ".L5" = f l5Language
  g _     = error $ "Error: bad L file: " ++ inputFile

cat :: FilePath -> IO ()
cat f = readFile f >>= putStrLn

ls :: FilePath -> IO [FilePath]
ls = getDirectoryContents
