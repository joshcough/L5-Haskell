module L.L1.L1 
  (
    CompilationMode(..)
   ,compileL1AndRunNative
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

data CompilationMode = L1Mode | L2Mode

compileL1 :: CompilationMode -> String -> Either String String
compileL1 mode code = (adjustStackInProgram mode . adjustMain <$> parseL1 (sread code)) >>= genX86Code

compileL1OrDie :: CompilationMode -> String -> String
compileL1OrDie mode = (either error id) . compileL1 mode

compileL1File_ :: CompilationMode -> FilePath -> IO String
compileL1File_ mode = compile1 (compileL1OrDie mode)

compileL1FileAndRunNative :: CompilationMode -> FilePath -> FilePath -> IO String
compileL1FileAndRunNative mode l1File outputDir = do
  s <- compileL1File_ mode l1File
  _   <- writeFile sFile s
  runNative sFile outputDir where 
  sFile = changeDir (changeExtension l1File "S") outputDir

-- the second argument is represents where the original code came from
-- maybe it came from an L5-L2 file. 
-- or, maybe it didn't come from a file at all
compileL1AndRunNative :: CompilationMode -> L1 -> Maybe FilePath -> FilePath -> IO String
compileL1AndRunNative mode l1 inputFile outputDir = do
  _   <- writeFile sFile s
  runNative sFile outputDir where 
  s = either error id $ genX86Code (adjustStackInProgram mode $ adjustMain l1)
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
adjustStackInProgram :: CompilationMode -> L1 -> L1
adjustStackInProgram L1Mode (Program main fs) =
  Program (adjustStackInFunction main) (fmap adjustStackInFunction fs)
adjustStackInProgram L2Mode p = p

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
