{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module L.Compiler (
   Val 
  ,ProgramName
  ,CompilationOptions(..)
  ,Language(..)
  ,OS(..)
  ,compile
  ,compileString
  ,compileFile
  ,compileAndRunNative
  ,compileAndRunNativeString
  ,compileAndRunNativeFile
  ,compileAndWriteResult
  ,compileFileAndWriteResult
  ,compOpts
  ,interpret
  ,interpretString
  ,interpretFile
  ,osFromString
  ,runVal
) where

import Control.Applicative
import Control.Category
import Data.Maybe
import Debug.Trace
import L.L1.L1Interp (Computer, showComputerOutput)
import L.IOHelpers
import L.OS
import L.Read
import L.NativeRunner
import Prelude hiding ((.),id)

data CompilationOptions = CompilationOptions {
  os :: OS
}
compOpts :: Maybe String -> CompilationOptions
compOpts = CompilationOptions . osFromMaybeString

type ProgramName      = String
type Val a            = Either String a
type Parser         i = SExpr -> Val i
type Compiler     i o = CompilationOptions -> ProgramName -> i -> Val o
type Interpreter    i = i -> Computer
type Extension        = String

data Language i o where
  Language :: (Show i, Show o) =>
    Parser i      ->
    Compiler  i o ->
    Interpreter i ->
    Extension     ->
    Maybe (Language o a) ->
    Language i o

parser       (Language p _ _ _ _) = p
compiler     (Language _ c _ _ _) = c
interpreter  (Language _ _ i _ _) = i
extension    (Language _ _ _ e _) = e

-- helpers
runVal :: Val a -> a
runVal = either error id

munge = either (return . Left)

outputExtension :: Language i o -> String
outputExtension (Language _ _ _ _ subLang) = maybe "S" extension subLang

tracer s = trace s (show s)

showOutput :: Language i o -> o -> String
showOutput (Language _ _ _ _ (Just l)) = show
showOutput Language{} = read . show

-- parsing
parseString :: Language i o -> String -> Val i
parseString l = liftParser $ parser l

parseFile :: Language i o -> FilePath -> IO (Val i)
parseFile l file = parseString l <$> readFile file

-- compilation
compile = compiler

compileString :: 
  Language i o       -> 
  ProgramName        -> 
  CompilationOptions -> 
  String             -> 
  Val o
compileString l name opts s = parseString l s >>= compile l opts name

compileFile :: 
  Language i o       -> 
  CompilationOptions -> 
  FilePath           -> 
  IO (Val o)
compileFile l opts file = readFile file >>= return . compileString l file opts

compileAndWriteResult :: Show o => 
  Language i o       -> 
  CompilationOptions -> 
  ProgramName        -> 
  FilePath           -> 
  i                  -> 
  IO (Val o)
compileAndWriteResult l opts name outputDir input = f $ compile l opts name input where
  f = munge $ \i -> writeOutput l name outputDir i >> return (Right i)

writeOutput :: Show o =>
  Language i o -> 
  ProgramName  -> 
  FilePath     -> 
  o            -> 
  IO ()
writeOutput l name outputDir code = 
  let codeFile = changeDir (name ++ "." ++ outputExtension l) outputDir
  in writeFile codeFile $ showOutput l code

compileFileAndWriteResult :: Show o => 
  Language i o       -> 
  CompilationOptions -> 
  FilePath           -> 
  FilePath           -> 
  IO (Val o)
compileFileAndWriteResult l opts outputDir inputFile = do
  i <- parseFile l inputFile
  munge (compileAndWriteResult l opts (getFileName inputFile) outputDir) i

-- interpretation
interpret = interpreter

interpretString :: Language i o -> String -> Val Computer
interpretString l s = interpret l <$> parseString l s

interpretFile :: Language i o -> FilePath -> IO (Val Computer)
interpretFile l file = readFile file >>= return . interpretString l

-- native execution
compileAndRunNative :: 
  Language i o       -> 
  CompilationOptions -> 
  ProgramName        -> 
  FilePath           -> 
  i                  -> 
  IO (Val String)
compileAndRunNative l@(Language _ _ _ _ subLang) opts name outputDir input = do
  code <- compileAndWriteResult l opts name outputDir input
  munge (recur subLang) code where
  recur Nothing code    = Right <$> runSCodeNative name outputDir (showOutput l code)
  recur (Just sub) code = compileAndRunNative sub opts name outputDir code

compileAndRunNativeString ::
  Language i o       -> 
  CompilationOptions ->
  ProgramName        -> 
  FilePath           -> 
  String             -> 
  IO (Val String)
compileAndRunNativeString lang opts name outputDir input =
  munge (compileAndRunNative lang opts name outputDir) (parseString lang input)

compileAndRunNativeFile ::
  Language i o       -> 
  CompilationOptions ->
  FilePath           -> 
  FilePath           -> 
  IO (Val String)
compileAndRunNativeFile lang opts outputDir inputFile = do
  code <- readFile inputFile
  compileAndRunNativeString lang opts (getFileName inputFile) outputDir code
