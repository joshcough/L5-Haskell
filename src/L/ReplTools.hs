module L.ReplTools (
   cat
  ,quickCompile
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
import System.FilePath.Lens

opts :: CompilationOptions
opts = def

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

