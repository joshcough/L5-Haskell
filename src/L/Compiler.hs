{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module L.Compiler {-(
   Val 
  ,ProgramName
  ,Compiler(..)
  ,Compiler1(..)
  ,CompilationOptions(..), os, outputDir
  ,Language(..)
  ,Language1(..)
  ,Thrist(..)
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
  ,interpretString
  ,interpretFile
  ,interpretTurtles
  ,interpretTurtlesString
  ,interpretTurtlesFile
  ,osFromString
  ,parseFile
  ,runVal
  ,munge
  ,withCompiler
) -} where

import Control.Applicative
import Control.Category
import Control.Lens
import Data.Default
import Data.Maybe
import L.OS
import L.Parser.SExpr
import L.Primitives (X86(..))
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
type Parser         i = SExpr -> Val i
type Interpreter    i = i -> Output
type Extension        = String

class (FromSExpr a, Show a) => FromSExprShow a
instance (FromSExpr a, Show a) => FromSExprShow a

data Compiler1 i o = Compiler1 {
  compile1 :: CompilationOptions -> ProgramName -> i -> Val o
 ,ext1     :: Extension
}

data Language1 i o where
  Language1 :: Compiler1 i o -> Interpreter i -> Language1 i o

data (p :=> f) i o where Constrained :: (p i, p o) => f i o -> (:=>) p f i o

data Thrist l i k where
  Nil  :: l i i -> Thrist l i i
  Cons :: l i j -> Thrist l j k -> Thrist l i k

mapThrist :: (forall x y. f x y -> g x y) -> Thrist f a b -> Thrist g a b
mapThrist f (Nil  lii)   = Nil  (f lii)
mapThrist f (Cons lij t) = Cons (f lij) (mapThrist f t)

type Compiler i o = Thrist (FromSExprShow :=> Compiler1) i o
type Language i o = Thrist (FromSExprShow :=> Language1) i o

constrain :: (p i, p o) => f i o -> (:=>) p f i o
constrain = Constrained

unconstrain :: Thrist (c :=> f) i o -> Thrist f i o
unconstrain = mapThrist (\(Constrained f) -> f)

unconstrain2 :: Thrist (c1 :=> (c2 :=> f)) i o -> Thrist f i o
unconstrain2 = mapThrist (\(Constrained (Constrained f)) -> f)

-- helpers
runVal :: Val a -> a
runVal = either error id

munge :: (b -> IO (Either a b1)) -> Either a b -> IO (Either a b1)
munge = either (return . Left)
mungeList :: (b -> [Either a b1]) -> Either a b -> [Either a b1]
mungeList = either (\msg -> [Left msg]) 

crawl :: FromSExprShow a => Thrist (FromSExprShow :=> f) a b -> (FromSExprShow b => r) -> r
crawl (Nil Constrained{}) r = r
crawl (Cons Constrained{} xs) r = crawl xs r

ext :: Compiler i o -> Extension
ext (Nil (Constrained (Compiler1 _ ext))) = ext
ext (Cons _ t)             = ext t

-- TODO: fmap on Constrained?
-- TODO: rename getCompiler?
toCompiler :: Language i o -> Compiler i o
toCompiler = mapThrist $ \(Constrained (Language1 c _)) -> (Constrained c)

-- TODO: rename getInterpreter?
toInterpreter :: Language i o -> Interpreter i
toInterpreter (Nil  (Constrained (Language1 _ i)))   = i
toInterpreter (Cons (Constrained (Language1 _ i)) _) = i

-- parsing
parseString :: Thrist (FromSExprShow :=> Compiler1) i o -> String -> Val i
parseString (Nil  (Constrained _))   s = fromString s
parseString (Cons (Constrained _) _) s = fromString s

parseFile :: Compiler i o -> FilePath -> IO (Val i)
parseFile (Nil  (Constrained _))   file = fromString <$> readFile file
parseFile (Cons (Constrained _) _) file = fromString <$> readFile file

-- compilation
compile :: Compiler i o -> CompilationOptions -> ProgramName -> i -> Either String o
compile (Nil  (Constrained (Compiler1 f _)))   opts name i = f opts name i
compile (Cons (Constrained (Compiler1 f _)) t) opts name i = f opts name i >>= compile t opts name

compileString ::
  FromSExpr i =>
  Compiler i o       ->
  ProgramName        ->
  CompilationOptions ->
  String             ->
  Val o
compileString c name opts s = fromString s >>= compile c opts name

compileFile ::
  FromSExpr i =>
  Compiler i o       ->
  CompilationOptions ->
  FilePath           ->
  IO (Val o)
compileFile c opts file = readFile file >>= return . compileString c file opts

compile1AndWriteResult :: Show o =>
  Compiler1 i o       ->
  CompilationOptions ->
  ProgramName        ->
  i                  ->
  IO (Val o)
compile1AndWriteResult (Compiler1 f ex) opts inputFile input = munge
  (\o -> do { _ <- writeFile (renameFile inputFile ex opts) (show o); return (return o) })
  (f opts inputFile input)

renameFile :: FilePath -> Extension -> CompilationOptions -> FilePath
renameFile inputFile ext opts = inputFile & extension .~ ext & directory .~ (getOutputDirOrElse opts inputFile)

compileAndWriteResult ::
  Compiler i o       ->
  CompilationOptions ->
  ProgramName        ->
  i                  ->
  IO (Val o)
compileAndWriteResult c opts inputFile input = f c where
  f (Nil  (Constrained (Compiler1 _ _)))   = munge go res
  f (Cons (Constrained (Compiler1 _ _)) t) = crawl t $ munge go res
  res = compile c opts inputFile input
  outputFile = renameFile inputFile (ext c) opts
  go :: Show o => o -> IO (Val o)
  go o = do {_ <- writeFile outputFile (show o); return (return o) }

writeOutput :: Show o =>
  Compiler i o ->
  ProgramName  ->
  FilePath     ->
  o            ->
  IO ()
writeOutput c inputFile outputDir code =
  writeFile (inputFile & extension .~ (ext c) & directory .~ outputDir) $ show code

compileFileAndWriteResult ::
  Compiler i o       ->
  CompilationOptions ->
  FilePath           ->
  IO (Val o)
compileFileAndWriteResult c opts inputFile = do
  i <- parseFile c inputFile
  munge (compileAndWriteResult c opts (inputFile^.filename)) i

-- interpretation
interpretString :: FromSExpr i =>  Interpreter i -> String -> Val Output
interpretString i s = i <$> fromString s

interpretFile :: FromSExpr i => Interpreter i -> FilePath -> IO (Val Output)
interpretFile i file = readFile file >>= return . interpretString i

-- interpret all levels.
-- TODO: gives the answers back in reverse order
interpretTurtles :: 
  Language i o   ->
  CompilationOptions ->
  ProgramName        -> 
  i                  ->
  [Val Output]
interpretTurtles (Nil (Constrained (Language1 _ i))) _ _ input = [Right $ i input]
interpretTurtles (Cons (Constrained (Language1 (Compiler1 f _) i)) t) opts name input =
  (Right $ i input) : (mungeList id $ do
    o <- f opts name input
    return $ interpretTurtles t opts name o)

interpretTurtlesString ::
  Language i o   ->
  CompilationOptions ->
  ProgramName        ->
  String             ->
  [Val Output]
interpretTurtlesString l opts name s =
  mungeList (interpretTurtles l opts name) (parseString (toCompiler l) s)

interpretTurtlesFile ::
  Language i o   ->
  CompilationOptions ->
  FilePath           ->
  IO ([Val Output])
interpretTurtlesFile t opts file = do
  code <- readFile file
  return $ interpretTurtlesString t opts file code

-- compile from i all the way to o, writing any intermediate files
compileTurtles ::
  Compiler i o ->
  CompilationOptions ->
  FilePath           -> --the original input file, also serves as the program name
  i                  ->
  IO ()
compileTurtles n@(Nil _)  opts inputFile input =
  compileAndWriteResult n opts inputFile input >> return ()
compileTurtles (Cons (Constrained c1) t) opts inputFile input = do
  code <- compile1AndWriteResult c1 opts inputFile input
  either error (compileTurtles t opts inputFile) code

compileTurtlesFile ::
 Compiler i o ->
 CompilationOptions ->
 FilePath           ->
 IO ()
compileTurtlesFile c opts file = do
  code <- parseFile c file
  either error (compileTurtles c opts file) code

-- native execution
compileAndRunNative ::
  Compiler i X86     ->
  CompilationOptions ->
  FilePath           -> --the original input file, also serves as the program name
  i                  ->
  IO (Val Output)
compileAndRunNative c opts inputFile input = do
  _ <- compileAndWriteResult c opts inputFile input
  return <$> runSFileNative sFile (sFile^.directory) where
    sFile = inputFile & extension .~ (ext c) & directory .~ (getOutputDirOrElse opts inputFile)

compileFileAndRunNative ::
  Compiler i X86     ->
  CompilationOptions ->
  FilePath           ->
  IO (Val Output)
compileFileAndRunNative c opts inputFile =
  parseFile c inputFile >>= munge (compileAndRunNative c opts inputFile)
