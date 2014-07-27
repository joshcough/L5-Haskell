{-# LANGUAGE GADTs #-}

module L.Compiler (
   Val 
  ,Language(..)
  ,compile
  ,compileString
  ,compileFile
  ,compileAndRunNative
  ,compileAndRunNativeString
  ,compileAndRunNativeFile
  ,interpret
  ,interpretString
  ,interpretFile
) where

import Control.Applicative
import Data.Maybe
import L.L1.L1Interp (Computer, showOutput)
import L.IOHelpers
import L.Read
import L.NativeRunner

type Val a               = Either String a
type Parser            i = SExpr -> Val i
type Compiler        i o = i -> Val o
type Interpreter       i = i -> Computer
type Extension           = String

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

-- parsing
parseString :: Language i o -> String -> Val i
parseString l = liftParser $ parser l

-- compilation
compile = compiler

compileString :: Language i o -> String -> Val o
compileString l s = parseString l s >>= compile l

compileFile :: Language i o -> FilePath -> IO (Val o)
compileFile l file = readFile file >>= return . compileString l

-- interpretation
interpret = interpreter

interpretString :: Language i o -> String -> Val Computer
interpretString l s = interpret l <$> parseString l s

interpretFile :: Language i o -> FilePath -> IO (Val Computer)
interpretFile l file = readFile file >>= return . interpretString l

-- native execution
compileAndRunNative :: Language i o -> String -> FilePath -> i -> IO (Val String)
compileAndRunNative l@(Language _ _ _ _ subLang) name outputDir input =
  f $ compile l input where
  f = either (return . Left) $ \code -> do
    _ <- writeFile codeFile (showCode code)
    maybe 
      (Right <$> runSCodeNative name outputDir (showCode code))
      (\sub -> compileAndRunNative sub name outputDir code)
      subLang where
  showCode code = maybe (read $ show code) (\_ -> show code) subLang
  codeFile = changeDir outputDir $ name ++ "." ++ maybe "S" extension subLang

compileAndRunNativeString :: Language i o -> String -> FilePath -> String -> IO (Val String)
compileAndRunNativeString lang name outputDir input = f $ parseString lang input where
  f = either (return . Left) (compileAndRunNative lang name outputDir)

compileAndRunNativeFile :: Language i o -> String -> FilePath -> FilePath -> IO (Val String)
compileAndRunNativeFile lang name outputDir inputFile = do
  code <- readFile inputFile
  compileAndRunNativeString lang name outputDir code

