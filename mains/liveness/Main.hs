module Main (main) where

import Data.List
import Data.Traversable
import L.Read
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.L2.L2
import L.L2.Liveness
import L.Utils
import System.Environment 
import System.IO

--main = fileArgMain (showLiveness . runLiveness)
main = livenessMain
