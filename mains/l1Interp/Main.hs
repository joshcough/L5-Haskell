module Main (main) where

import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L1.L1Interp

main = interpL1File

interpL1File = 
  do s <- withFileArgT $ \_ code -> runVal $ interpretString l1Language code
     putStrLn . showComputerOutput $ snd s
