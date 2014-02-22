module L.L1.L1 
  (
    compileL1AndRunNative
   ,compileL1FileAndRunNative
   ,compileL1
   ,compileL1OrDie
   --,compileL1File
   ,compileL1File_
   ,runNative
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Traversable
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.Read
import L.Utils
import L.L1.L1X86
import System.Cmd
import System.Environment 
import System.IO

compileL1 :: Bool -> String -> Either String String
compileL1 adjustStack code = (adjustStackInProgram adjustStack . adjustMain <$> parseL1 (sread code)) >>= genX86Code

compileL1OrDie :: Bool -> String -> String
compileL1OrDie adjustStack = (either error id) . compileL1 adjustStack

compileL1File_ :: Bool -> FilePath -> IO String
compileL1File_ adjustStack = compile1 (compileL1OrDie adjustStack)

compileL1FileAndRunNative :: Bool -> FilePath -> FilePath -> IO String
compileL1FileAndRunNative adjustStack l1File outputDir = do
  s <- compileL1File_ adjustStack l1File
  _   <- writeFile sFile s
  runNative sFile outputDir where 
  sFile = changeDir (changeExtension l1File "S") outputDir

-- the second argument is represents where the original code came from
-- maybe it came from an L5-L2 file. 
-- or, maybe it didn't come from a file at all
compileL1AndRunNative :: Bool -> L1 -> Maybe FilePath -> FilePath -> IO String
compileL1AndRunNative adjustStack l1 inputFile outputDir = do
  _   <- writeFile sFile s
  runNative sFile outputDir where 
  s = either error id $ genX86Code (adjustStackInProgram adjustStack $ adjustMain l1)
  sFile = case inputFile of
    Just f  -> changeDir (changeExtension f "S") outputDir
    Nothing -> outputDir ++ "tmp.S"

runNative :: FilePath -> FilePath -> IO String
runNative sFile outputDir = 
  let f newExt = changeDir (changeExtension sFile newExt) outputDir
      oFile   = f "S.o"
      outFile = f "S.out"
      resFile = f "S.res"
  in do
    _ <- rawSystem "as"  ["-o", oFile,   sFile]
    _ <- rawSystem "gcc" ["-o", outFile, oFile, "bin/runtime.o"]
    _ <- rawSystem "bin/run.sh" [outFile, resFile]
    readFile resFile

{-
  if we need to adjust the stack then
    -  we need to decrement the stack pointer when we enter the function
    -  whenever we return, we must make sure to put the stack pointer back.
       (by incrementing it by the amount we decremented it by 8)
       rewriteReturns puts that increment in front of all returns in the function.
 -}
adjustStackInProgram :: Bool -> L1 -> L1
adjustStackInProgram True (Program main fs) =
  Program (adjustStackInFunction main) (fmap adjustStackInFunction fs)
adjustStackInProgram False p = p

adjustStackInFunction :: L1Func -> L1Func
adjustStackInFunction (Func (labl : insts)) =
  Func $ concat [[labl, decEsp], rewriteReturns insts] where
    decEsp = MathInst rsp decrement (NumberL1S 8)
    incEsp = MathInst rsp increment (NumberL1S 8)
    rewriteReturns insts = insts >>= f where
      f r@Return = [incEsp, r]
      f i        = [i]

adjustMain :: L1 -> L1
adjustMain (Program main fs) = Program (adjustMain_ main) fs
adjustMain_ :: L1Func -> L1Func
adjustMain_ (Func body) = Func $ concat [mainLabel : mainBody ++ [Return]] where
  mainLabel = LabelDeclaration "main"
  mainBody = stripLabel (reverse $ stripReturn $ reverse body) where
    stripLabel is@(LabelDeclaration "main" : rest) = rest
    stripLabel is = is
    stripReturn (Return : rest) = stripReturn rest
    stripReturn rest = rest
