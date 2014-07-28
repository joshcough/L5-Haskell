module Main (main) where

import L.L2.L2


import L.Compiler
import L.IOHelpers
import L.L2.L2
import L.L1.L1Interp

main = interpL2File

interpL2File = 
  do s <- withFileArgT $ \_ code -> runVal $ interpretString l2Language code
     putStrLn . showComputerOutput $ snd s
