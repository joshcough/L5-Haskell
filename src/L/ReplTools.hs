{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module L.ReplTools (
   quickCompile
  ,quickCompileTurtles
) where

import Data.Default
import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L2.L2
import L.L3.L3
import L.L4.L4

opts :: CompilationOptions
opts = def

-- TODO: really need a pretty printer for displaying the result in the repl
quickCompile :: Show o => FilePath -> IO String
quickCompile inputFile = g (getExtension inputFile) where
  g "L1" = compileFileAndWriteResult l1Language opts inputFile >>= return . show
  g "L2" = compileFileAndWriteResult l2Language opts inputFile >>= return . show
  g "L3" = compileFileAndWriteResult l3Language opts inputFile >>= return . show
  g "L4" = compileFileAndWriteResult l4Language opts inputFile >>= return . show
  g _    = error $ "Error: bad L file: " ++ inputFile

quickCompileTurtles :: FilePath -> IO ()
quickCompileTurtles inputFile = g (getExtension inputFile) where
  g "L1" = compileTurtlesFile l1Language opts inputFile
  g "L2" = compileTurtlesFile l2Language opts inputFile
  g "L3" = compileTurtlesFile l3Language opts inputFile
  g "L4" = compileTurtlesFile l4Language opts inputFile
  g _    = error $ "Error: bad L file: " ++ inputFile