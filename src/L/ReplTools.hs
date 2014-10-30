{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module L.ReplTools (
   quickCompile
  ,quickCompileTurtles
  ,quickInterp
  ,quickRunNative
  ,showFile
) where

import Control.Applicative
import Control.Lens
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
quickCompile :: Show o => FilePath -> IO String
quickCompile inputFile = g $ inputFile^.extension where
  g ".L1" = compileFileAndWriteResult l1Language opts inputFile >>= return . show
  g ".L2" = compileFileAndWriteResult l2Language opts inputFile >>= return . show
  g ".L3" = compileFileAndWriteResult l3Language opts inputFile >>= return . show
  g ".L4" = compileFileAndWriteResult l4Language opts inputFile >>= return . show
  g ".L5" = compileFileAndWriteResult l5Language opts inputFile >>= return . show
  g _    = error $ "Error: bad L file: " ++ inputFile

quickCompileTurtles :: FilePath -> IO ()
quickCompileTurtles inputFile = g $ inputFile^.extension where
  g ".L1" = compileTurtlesFile l1Language opts inputFile
  g ".L2" = compileTurtlesFile l2Language opts inputFile
  g ".L3" = compileTurtlesFile l3Language opts inputFile
  g ".L4" = compileTurtlesFile l4Language opts inputFile
  g ".L5" = compileTurtlesFile l5Language opts inputFile
  g _    = error $ "Error: bad L file: " ++ inputFile

quickRunNative :: Show o => FilePath -> IO String
quickRunNative inputFile = g $ inputFile^.extension where
  g ".L1" = compileFileAndRunNative l1Language opts inputFile >>= return . show
  g ".L2" = compileFileAndRunNative l2Language opts inputFile >>= return . show
  g ".L3" = compileFileAndRunNative l3Language opts inputFile >>= return . show
  g ".L4" = compileFileAndRunNative l4Language opts inputFile >>= return . show
  g _    = error $ "Error: bad L file: " ++ inputFile

quickInterp :: Show o => FilePath -> IO (Either String String)
quickInterp inputFile = g $ inputFile^.extension where
  g ".L1" = interpretFile l1Language inputFile
  g ".L2" = interpretFile l2Language inputFile
  g ".L3" = interpretFile l3Language inputFile
  g ".L4" = interpretFile l4Language inputFile
  g ".L5" = interpretFile l5Language inputFile
  g _    = error $ "Error: bad L file: " ++ inputFile

showFile :: FilePath -> IO ()
showFile f = lines <$> readFile f >>= mapM_ putStrLn
