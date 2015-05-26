{-# LANGUAGE TypeOperators #-}

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
  ,quickParse
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
import L.Parser.SExpr
import L.Primitives (X86)
import System.Directory
import System.FilePath.Lens

opts :: CompilationOptions
opts = def

quickCompileString :: (FromSExpr i, Show o) => Compiler i o -> String -> String
quickCompileString l s = show . runVal $ compileString l s opts s

quickCompileL1 :: String -> String
quickCompileL1 = quickCompileString l1Compiler

quickCompileL2 :: String -> String
quickCompileL2 = quickCompileString l2Compiler

quickCompileL3 :: String -> String
quickCompileL3 = quickCompileString l3Compiler

quickCompileL4 :: String -> String
quickCompileL4 = quickCompileString l4Compiler

quickCompileL5 :: String -> String
quickCompileL5 = quickCompileString l5Compiler

-- TODO: really need a pretty printer for displaying the result in the repl
quickCompile :: FilePath -> IO ()
quickCompile inputFile = error "todo" {-
g $ inputFile^.extension  where
  f :: (FromSExpr i, Show o) => Compiler i o -> IO ()
  f c = liftM show (compileFileAndWriteResult c opts inputFile) >>= putStrLn
  g ".L1" = f l1Compiler
  g ".L2" = f l2Compiler
  g ".L3" = f l3Compiler
  g ".L4" = f l4Compiler
  g ".L5" = f l5Compiler
  g _     = error $ "Error: bad L file: " ++ inputFile
-}

quickCompileTurtles :: FilePath -> IO ()
quickCompileTurtles inputFile = error "todo" {-
g $ inputFile^.extension where
  f :: (FromSExpr i, Show i, Show o) => Compiler i o -> IO ()
  f c = compileTurtlesFile (mapThrist Constrained c) opts inputFile
  g ".L1" = f l1Compiler
  --g ".L2" = f $ error "todo" --l2Compiler
  --g ".L3" = f $ error "todo" --l3Compiler
  --g ".L4" = f $ error "todo" --l4Compiler
  --g ".L5" = f $ error "todo" --l5Compiler
  g _     = error $ "Error: bad L file: " ++ inputFile
-}

quickRunNative :: FilePath -> IO ()
quickRunNative inputFile = g $ inputFile^.extension  where
  f :: FromSExpr i => Compiler i X86 -> IO ()
  f l = liftM show (compileFileAndRunNative l opts inputFile) >>= putStrLn
  g ".L1" = f l1Compiler
  g ".L2" = f l2Compiler
  g ".L3" = f l3Compiler
  g ".L4" = f l4Compiler
  g ".L5" = f l5Compiler
  g _     = error $ "Error: bad L file: " ++ inputFile

quickInterp :: FilePath -> IO ()
quickInterp inputFile = g $ inputFile^.extension where
  f :: FromSExpr i => Language1 i o -> IO ()
  f (Language1 _ i) = either id id <$> interpretFile i inputFile >>= putStrLn
  g ".L1" = f l1Language1
  g ".L2" = f l2Language1
  g ".L3" = f l3Language1
  g ".L4" = f l4Language1
  g ".L5" = f l5Language1
  g _     = error $ "Error: bad L file: " ++ inputFile

quickParse :: FilePath -> IO ()
quickParse inputFile = error "todo" {-
g $ inputFile^.extension where
  f :: (FromSExpr i, Show i) => Language i o -> IO ()
  f l = parseFile inputFile >>= putStrLn . show
  g ".L1" = f l1Language
  g ".L2" = f l2Language
  g ".L3" = f l3Language
  g ".L4" = f l4Language
  g ".L5" = f l5Language
  g _     = error $ "Error: bad L file: " ++ inputFile
-}

cat :: FilePath -> IO ()
cat f = readFile f >>= putStrLn

ls :: FilePath -> IO [FilePath]
ls = getDirectoryContents
