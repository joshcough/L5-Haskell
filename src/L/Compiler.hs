{-# LANGUAGE GADTs #-}

module L.Compiler where

import Control.Applicative
import L.L1.L1Interp (Computer, showOutput)
import L.Read

type Val a               = Either String a
type Parser            i = SExpr -> Val i
type Compiler        i o = i -> Val o
type Interpreter       i = i -> Computer
type NativeRunner      o = String -> FilePath -> o -> IO (Val String) 

data Language i o where
  Language :: (Show i, Show o) =>
    Parser i -> Compiler  i o -> Interpreter i -> NativeRunner o -> Language i o

parser       (Language p _ _ _) = p
compiler     (Language _ c _ _) = c
interpreter  (Language _ _ i _) = i
nativeRunner (Language _ _ _ n) = n

-- parsing
parseString :: Language i o -> String -> Val i
parseString l = liftParser $ parser l

-- compilation
compileString :: Language i o -> String -> Val o
compileString l s = parseString l s >>= compiler l

compileFile :: Language i o -> FilePath -> IO (Val o)
compileFile l file = readFile file >>= return . compileString l

{-
-- interpretation
compileAndInterpret :: Language i o -> i -> Val Computer
compileAndInterpret l i = interpreter l <$> compiler l i

compileAndInterpretString :: Language i o -> String -> Val Computer
compileAndInterpretString l s = parseString l s >>= compileAndInterpret l 

compileAndInterpretFile :: Language i o -> FilePath -> IO (Val Computer)
compileAndInterpretFile l = withFile $ compileAndInterpretString l

type NativeRunner      o = String -> FilePath -> o -> IO (Val String)

-}

-- native execution
compileAndRunNative :: Language i o -> String -> FilePath -> i -> IO (Val String)
compileAndRunNative lang name outputDir input = f $ compiler lang input where
  f = either (return . Left) (nativeRunner lang name outputDir)

compileAndRunNativeString :: Language i o -> String -> FilePath -> String -> IO (Val String)
compileAndRunNativeString lang name outputDir input = f $ parseString lang input where
  f = either (return . Left) (compileAndRunNative lang name outputDir)

compileAndRunNativeFile :: Language i o -> String -> FilePath -> FilePath -> IO (Val String)
compileAndRunNativeFile lang name outputDir inputFile = do
  code <- readFile inputFile
  compileAndRunNativeString lang name outputDir code

