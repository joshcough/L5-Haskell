{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-
module L.Compiler (
   Val 
  ,Language(..)
  ,compile
  ,compileString
  ,compileFile
  ,compileAndRunNative
  ,compileAndRunNativeString
  ,compileFileAndRunNative
  ,compileAndWriteResult
  ,compileFileAndWriteResult
  ,interpret
  ,interpretString
  ,interpretFile
  ,runVal
) where

import Control.Applicative
import Control.Category
import Data.Maybe
import L.L1.L1Interp (Computer, showComputerOutput)
import L.IOHelpers
import L.Read
import L.NativeRunner
import Debug.Trace
import Prelude hiding ((.),id)

type Val a            = Either String a
type Parser         i = SExpr -> Val i
type Compiler     i o = i -> Val o
type Interpreter    i = i -> Computer
type Extension        = String

data Thrist l i k where
  Nil :: Thrist l i i
  Snoc :: Thrist l j k -> l i j -> Thrist l i k

instance Category (Thrist l) where
  id = Nil
  xs . Nil = xs
  xs . Snoc ys y = undefined
  -- ...

class Cat t where
  foldCat :: Category q => (forall a b. p a b -> q a b) -> t p a b -> q a b
  mapCat :: (forall a b. p a b -> q a b) -> t p a b -> t q a b
  traverseCat :: Applicative m => (forall a b. p a b -> m (q a b)) -> t p a b -> m (t q a b)

instance Cat Thrist where
  foldCat f Nil = id
  foldCat f (Snoc xs x) = foldCat f xs . f x
  mapCat f Nil = Nil
  mapCat f (Snoc xs x) = Snoc (mapCat f xs) (f x)
  traverseCat f Nil = pure Nil
  traverseCat f (Snoc xs x) = Snoc <$> traverseCat f xs <*> f x

foo :: Language i o -> Kleisli m i o
runKleisli (foldCat foo) :: Thrist Language i o -> i -> m o

foldCat foo :: Thrist Language i o -> i -> o

data Language i o where
  Language :: -- (Show i, Show o) =>
    Parser i      ->
    Compiler  i o ->
    Interpreter i ->
    Extension     ->
    Language i o

instance Functor (Language i) where
instance Semigroupoid Language where
  Language _ c' _ _ `o` Language p c i e = Language p c'' i e where
     c'' = c >=> c'

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

compileString :: Language i o -> String -> Val o
compileString l s = parseString l s >>= compile l

compileFile :: Language i o -> FilePath -> IO (Val o)
compileFile l file = readFile file >>= return . compileString l

compileAndWriteResult :: Show o => Language i o -> String -> FilePath -> i -> IO (Val o)
compileAndWriteResult l name outputDir input = f $ compile l input where
  f = munge $ \i -> writeOutput l name outputDir i >> return (Right i)

writeOutput :: Show o => Language i o -> String -> FilePath -> o -> IO ()
writeOutput l name outputDir code = 
  let codeFile = changeDir (name ++ "." ++ outputExtension l) outputDir
  in writeFile codeFile $ showOutput l code

compileFileAndWriteResult :: Show o => Language i o -> FilePath -> FilePath -> IO (Val o)
compileFileAndWriteResult l outputDir inputFile = do
  i <- parseFile l inputFile
  munge (compileAndWriteResult l (getFileName inputFile) outputDir) i

-- interpretation
interpret = interpreter

interpretString :: Language i o -> String -> Val Computer
interpretString l s = interpret l <$> parseString l s

interpretFile :: Language i o -> FilePath -> IO (Val Computer)
interpretFile l file = readFile file >>= return . interpretString l

-- native execution
compileAndRunNative :: Language i o -> String -> FilePath -> i -> IO (Val String)
compileAndRunNative l@(Language _ _ _ _ subLang) name outputDir input = do
  code <- compileAndWriteResult l name outputDir input
  munge (recur subLang) code where
  recur Nothing code    = Right <$> runSCodeNative name outputDir (showOutput l code)
  recur (Just sub) code = compileAndRunNative  sub name outputDir code

compileAndRunNativeString :: Language i o -> String -> FilePath -> String -> IO (Val String)
compileAndRunNativeString lang name outputDir input =
  munge (compileAndRunNative lang name outputDir) (parseString lang input)

compileFileAndRunNative :: Language i o -> FilePath -> FilePath -> IO (Val String)
compileFileAndRunNative lang outputDir inputFile = do
  code <- readFile inputFile
  compileAndRunNativeString lang (getFileName inputFile) outputDir code
-}