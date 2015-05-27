{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
import L.Runners
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
quickCompile inputFile = withCompiler (inputFile^.extension) $ \c ->
  liftM show (compileFileAndWriteResult c opts inputFile) >>= putStrLn

quickCompileTurtles :: FilePath -> IO ()
quickCompileTurtles inputFile =
  withCompiler (inputFile^.extension) (\c -> compileTurtlesFile c opts inputFile)

quickRunNative :: FilePath -> IO ()
quickRunNative inputFile = withCompiler (inputFile^.extension) f where
  f :: FromSExpr i => Compiler i X86 -> IO ()
  f c@(Nil  (Constrained _)  ) = liftM show (compileFileAndRunNative c opts inputFile) >>= putStrLn
  f c@(Cons (Constrained _) _) = liftM show (compileFileAndRunNative c opts inputFile) >>= putStrLn

quickInterp :: FilePath -> IO ()
quickInterp inputFile = withLanguage1 (inputFile^.extension) f where
  f :: FromSExpr i => Language1 i o -> IO ()
  f (Language1 _ i) = either id id <$> interpretFile i inputFile >>= putStrLn

quickParse :: FilePath -> IO ()
quickParse inputFile = withCompiler (inputFile^.extension) f where
  f :: FromSExpr i => Compiler i o -> IO ()
  f c@(Nil  (Constrained _)  ) = parseFile c inputFile >>= putStrLn . show
  f c@(Cons (Constrained _) _) = parseFile c inputFile >>= putStrLn . show

cat :: FilePath -> IO ()
cat f = readFile f >>= putStrLn

ls :: FilePath -> IO [FilePath]
ls = getDirectoryContents
