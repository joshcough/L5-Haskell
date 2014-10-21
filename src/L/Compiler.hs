{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module L.Compiler (
   Val 
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
  ,runVal
  ,munge
) where

import Control.Applicative
import Control.Category
import Control.Lens
import Data.Default
import Data.Maybe
import L.LCompiler
import L.OS
import L.Read
import L.NativeRunner
import Prelude hiding ((.),id)
import System.FilePath.Lens

type Val a            = Either String a
type Parser         i = SExpr -> Val i
type Compiler     i o = CompilationOptions -> ProgramName -> i -> Val o
type Interpreter    i = i -> Output
type Extension        = String

data Language i o where
  Language :: (Show i, Show o) =>
    Parser i      ->
    Compiler  i o ->
    Interpreter i ->
    Extension     ->
    Maybe (Language o a) ->
    Language i o

parser      :: Language i o -> Parser i
parser        (Language p _ _ _ _) = p
compiler    :: Language i o -> Compiler i o
compiler      (Language _ c _ _ _) = c
interpreter :: Language i o -> Interpreter i
interpreter   (Language _ _ i _ _) = i
ext         :: Language i o -> Extension
ext           (Language _ _ _ e _) = e

-- helpers
runVal :: Val a -> a
runVal = either error id

munge :: (b -> IO (Either a b1)) -> Either a b -> IO (Either a b1)
munge = either (return . Left)
mungeList :: (b -> [Either a b1]) -> Either a b -> [Either a b1]
mungeList = either (\msg -> [Left msg]) 

outputExtension :: Language i o -> String
outputExtension (Language _ _ _ _ subLang) = maybe "S" ext subLang

showOutput :: Language i o -> o -> String
showOutput (Language _ _ _ _ (Just _)) = show
showOutput Language{} = read . show

-- parsing
parseString :: Language i o -> String -> Val i
parseString l = liftParser $ parser l

parseFile :: Language i o -> FilePath -> IO (Val i)
parseFile l file = parseString l <$> readFile file

-- compilation
compile :: Language i o -> Compiler i o
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
  i                  ->
  IO (Val o)
compileAndWriteResult l opts inputFile input =
  f $ compile l opts inputFile input where
    f = munge $ \o -> do
      _ <- writeOutput l inputFile (getOutputDirOrElse opts inputFile) o
      return (Right o)

writeOutput :: Show o =>
  Language i o -> 
  ProgramName  -> 
  FilePath     -> 
  o            -> 
  IO ()
writeOutput l inputFile outputDir code = do
  writeFile outputFile $ showOutput l code where
    outputFile = inputFile & extension .~ (outputExtension l) & directory .~ outputDir

compileFileAndWriteResult :: Show o => 
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

interpretString :: Language i o -> String -> Val Output
interpretString l s = interpret l <$> parseString l s

interpretFile :: Language i o -> FilePath -> IO (Val Output)
interpretFile l file = readFile file >>= return . interpretString l

-- interpret all levels.
-- TODO: gives the answers back in reverse order
interpretTurtles :: 
  Language i o       -> 
  CompilationOptions -> 
  ProgramName        -> 
  i                  ->
  [Val Output]
interpretTurtles l@(Language _ _ _ _ subLang) opts name input = 
  (Right $ interpret l input) : (maybe [] f subLang) where
    f sub = mungeList id $ do
      i' <- compile l opts name input
      return $ interpretTurtles sub opts name i'

interpretTurtlesString ::
  Language i o       ->
  CompilationOptions ->
  ProgramName        ->
  String             ->
  [Val Output]
interpretTurtlesString l opts name s =
  mungeList (interpretTurtles l opts name) (parseString l s)

interpretTurtlesFile ::
  Language i o       ->
  CompilationOptions ->
  FilePath           ->
  IO ([Val Output])
interpretTurtlesFile l opts file = do
  code <- readFile file
  return $ interpretTurtlesString l opts file code

-- compile all the way to assembly, writing intermediate files
compileTurtles ::
  Language i o       ->
  CompilationOptions ->
  FilePath           -> --the original input file, also serves as the program name
  i                  ->
  IO ()
compileTurtles l@(Language _ _ _ _ subLang) opts inputFile input = do
  code <- compileAndWriteResult l opts inputFile input
  case (subLang, code) of
    (_, Left err)       -> error err
    (Nothing, _)        -> return ()
    (Just sub, Right o) -> compileTurtles sub opts inputFile o

compileTurtlesFile ::
 Language i o       ->
 CompilationOptions ->
 FilePath           ->
 IO ()
compileTurtlesFile l opts file = do
  code <- parseFile l file
  either error (compileTurtles l opts file) code

-- native execution
compileAndRunNative ::
  Language i o       ->
  CompilationOptions ->
  FilePath           -> --the original input file, also serves as the program name
  i                  ->
  IO (Val Output)
compileAndRunNative l@(Language _ _ _ _ subLang) opts inputFile input = do
  code <- compileAndWriteResult l opts inputFile input
  munge (recur subLang) code where
  recur Nothing    _    = Right <$> runSFileNative sFile (sFile^.directory) where
    sFile = inputFile & extension .~ (outputExtension l) & directory .~ (getOutputDirOrElse opts inputFile)
  recur (Just sub) code = compileAndRunNative sub opts inputFile code

compileFileAndRunNative ::
  Language i o       -> 
  CompilationOptions ->
  FilePath           ->
  IO (Val Output)
compileFileAndRunNative lang opts inputFile = do
  code <- readFile inputFile
  munge (compileAndRunNative lang opts inputFile) (parseString lang code)
