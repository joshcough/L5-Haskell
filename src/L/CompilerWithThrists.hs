{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-
module L.Compiler where

import Control.Applicative
import Control.Category
import Data.Maybe
import L.L1.L1Interp (Computer, showComputerOutput)
import L.Util.IOHelpers
import L.Parser.SExpr
import L.Util.NativeRunner
import Debug.Trace
import Prelude hiding ((.),id)

type Val a            = Either String a
type Parser         i = SExpr -> Val i
type Compiler     i o = i -> Val o
type Interpreter    i = i -> Computer
type Extension        = String

data Language i o where
  Language :: (Show i, Show o) =>
    Compiler  i o ->
    Interpreter i ->
    Language i o

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



{-
5/25/15 Half-hearted and failed attempt

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module L.Compiler (
   Val
  ,ProgramName
  ,CompilationOptions(..), os, outputDir
  ,Language(..)
  ,compile
  ,compileString
  ,compileFile
  ,compileAndRunNative
  ,compileFileAndRunNative
  ,compileAndWriteResult
  ,compileFileAndWriteResult
  ,compileTurtles
  ,compileTurtlesFile
  ,compOpts
  ,ext
  ,interpret
  ,interpretString
  ,interpretFile
  ,interpretTurtles
  ,interpretTurtlesString
  ,interpretTurtlesFile
  ,osFromString
  ,parseFile
  ,fromString
  ,runVal
  ,munge
) where

import Control.Applicative
import Control.Category
import Control.Lens
import Data.Default
import Data.Maybe
import L.OS
import L.Parser.SExpr
import L.Util.NativeRunner
import Prelude hiding ((.),id)
import System.FilePath.Lens

data CompilationOptions = CompilationOptions {
  _os :: OS,
  _outputDir :: Maybe FilePath
} deriving (Show, Eq)
makeLenses ''CompilationOptions

compOpts :: Maybe String -> Maybe FilePath -> CompilationOptions
compOpts mOs mOutputDir =
  CompilationOptions (osFromMaybeString mOs) mOutputDir

-- this sets it only if it isnt there
getOutputDirOrElse :: CompilationOptions -> FilePath -> FilePath
getOutputDirOrElse opts inputFile = fromJust $ setIfAbsent^.outputDir where
  setIfAbsent :: CompilationOptions
  setIfAbsent = opts & outputDir %~ (<|> (Just $ inputFile^.directory))

instance Default CompilationOptions where
  def = CompilationOptions systemOS Nothing

type Output           = String
type ProgramName      = String
type Val a            = Either String a
type Compiler     i o = CompilationOptions -> ProgramName -> i -> Val o
type Interpreter    i = i -> Output
type Extension        = String

data Thrist l i k where
  MkThrist   :: l i i -> Thrist l i i
  ConsThrist :: l i j -> Thrist l j k -> Thrist l i k

data Language i o where
  Language :: (Show i, Show o) =>
    Compiler  i o ->
    Interpreter i ->
    Extension     ->
    Language i o

compiler    :: Language i o -> Compiler i o
compiler      (Language c _ _) = c
interpreter :: Language i o -> Interpreter i
interpreter   (Language _ i _) = i
ext         :: Language i o -> Extension
ext           (Language _ _ e) = e

outputExtension :: Thrist Language i o -> Extension
outputExtension (MkThrist c)     = ext c
outputExtension (ConsThrist c _) = ext c

-- helpers
runVal :: Val a -> a
runVal = either error id

{-
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

munge' :: (b -> m1 (m2 a b1)) -> m2 a b -> m1 (m2 a b1)
munge' = either (return . Left)

munge' :: (a -> m1 (m2 b)) -> m2 a -> m1 (m2 b)
munge' = ???
-}

munge :: (b -> IO (Either a b1)) -> Either a b -> IO (Either a b1)
munge = either (return . Left)
mungeList :: (b -> [Either a b1]) -> Either a b -> [Either a b1]
mungeList = either (\msg -> [Left msg])

-- parsing
parseFile :: FromSExpr i => Language i o -> FilePath -> IO (Val i)
parseFile l file = fromString <$> readFile file

-- compilation
compile :: Thrist Language i o -> Compiler i o
compile = compiler

compileString ::
  FromSExpr i =>
  Thrist Language i o ->
  ProgramName         ->
  CompilationOptions  ->
  String              ->
  Val o
compileString l name opts s = fromString s >>= compile l opts name

compileFile ::
  FromSExpr i =>
  Language i o       ->
  CompilationOptions ->
  FilePath           ->
  IO (Val o)
compileFile l opts file = readFile file >>= return . compileString l file opts

compileAndWriteResult :: Show o =>
  Language i o       ->
  CompilationOptions ->
  ProgramName        ->
  i                  ->
  IO (Val o)
compileAndWriteResult l opts inputFile input =
  f $ compile l opts inputFile input where
    f = munge $ \o -> do
      _ <- writeOutput l inputFile (getOutputDirOrElse opts inputFile) o
      return (return o)

writeOutput :: Show o =>
  Thrist Language i o ->
  ProgramName  ->
  FilePath     ->
  o            ->
  IO ()
writeOutput l inputFile outputDir code = do
  writeFile outputFile $ show code where
    outputFile = inputFile & extension .~ (outputExtension l) & directory .~ outputDir

compileFileAndWriteResult :: (FromSExpr i, Show o) =>
  Language i o       ->
  CompilationOptions ->
  FilePath           ->
  IO (Val o)
compileFileAndWriteResult l opts inputFile = do
  i <- parseFile l inputFile
  munge (compileAndWriteResult l opts (inputFile^.filename)) i

-- interpretation
interpret :: Language i o -> Interpreter i
interpret = interpreter

interpretString :: FromSExpr i => Language i o -> String -> Val Output
interpretString l s = interpret l <$> fromString s

interpretFile :: FromSExpr i => Language i o -> FilePath -> IO (Val Output)
interpretFile l file = readFile file >>= return . interpretString l

-- interpret all levels.
-- TODO: gives the answers back in reverse order
interpretTurtles ::
  Language i o       ->
  CompilationOptions ->
  ProgramName        ->
  i                  ->
  [Val Output]
interpretTurtles l@(Language _ _ subLang) opts name input =
  (Right $ interpret l input) : (maybe [] f subLang) where
    f sub = mungeList id $ do
      i' <- compile l opts name input
      return $ interpretTurtles sub opts name i'

interpretTurtlesString ::
  FromSExpr i =>
  Language i o       ->
  CompilationOptions ->
  ProgramName        ->
  String             ->
  [Val Output]
interpretTurtlesString l opts name s =
  mungeList (interpretTurtles l opts name) (fromString s)

interpretTurtlesFile ::
  FromSExpr i =>
  Language i o       ->
  CompilationOptions ->
  FilePath           ->
  IO ([Val Output])
interpretTurtlesFile l opts file = do
  code <- readFile file
  return $ interpretTurtlesString l opts file code

-- compile all the way to assembly, writing intermediate files
compileTurtles ::
  (FromSExpr i, FromSExpr o) =>
  Language i o       ->
  CompilationOptions ->
  FilePath           -> --the original input file, also serves as the program name
  i                  ->
  IO ()
compileTurtles l@(Language _ _ subLang) opts inputFile input = do
  code <- compileAndWriteResult l opts inputFile input
  case (subLang, code) of
    (_, Left err)       -> error err
    (Nothing, _)        -> return ()
    (Just sub, Right o) -> compileTurtles sub opts inputFile o

compileTurtlesFile ::
 (FromSExpr i, FromSExpr o) =>
 Language i o       ->
 CompilationOptions ->
 FilePath           ->
 IO ()
compileTurtlesFile l opts file = do
  code <- parseFile l file
  either error (compileTurtles l opts file) code

-- native execution
compileAndRunNative ::
  (FromSExpr i, FromSExpr o) =>
  Thrist Language i o ->
  CompilationOptions ->
  FilePath           -> --the original input file, also serves as the program name
  i                  ->
  IO (Val Output)
compileAndRunNative l@(Language _ _ subLang) opts inputFile input = do
  code <- compileAndWriteResult l opts inputFile input
  munge (recur subLang) code where
  recur Nothing    _    = return <$> runSFileNative sFile (sFile^.directory) where
    sFile = inputFile & extension .~ (outputExtension l) & directory .~ (getOutputDirOrElse opts inputFile)
  recur (Just sub) code = compileAndRunNative sub opts inputFile code

compileFileAndRunNative ::
  (FromSExpr i, FromSExpr o) =>
  Language i o       ->
  CompilationOptions ->
  FilePath           ->
  IO (Val Output)
compileFileAndRunNative lang opts inputFile = do
  code <- readFile inputFile
  munge (compileAndRunNative lang opts inputFile) (fromString code)


-}