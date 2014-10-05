module L.L3.L3Interp (
   evalL3
  ,interpL3
  ,runInterpL3
) where

import Control.Applicative
import Control.Monad.State
import Data.Array.IO (IOArray)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Debug.Trace
import L.L1L2AST hiding (Func)
import L.L3.L3AST
import L.Utils
import System.IO.Unsafe

import qualified Data.Array.IO as IOArray

data Runtime = 
    Number Int64
  | Pointer Int64
  | FunctionPointer Label deriving (Show, Eq)
type Output = [String]
type Mem = (IOArray Int64 Runtime, Int64) -- heap, and heap pointer
type Env = Map Variable Runtime
type Lib = Map Label Func
type M x = StateT L3Computer IO x

data L3Computer = L3Computer {
  env    :: Env
 ,mem    :: Mem
 ,output :: Output
 ,lib    :: Lib
}

-- state functions
-- | get the env out of the state
getEnv :: M Env
getEnv = env <$> get
-- | get the library out of the state
getLib :: M Lib
getLib = lib <$> get
-- | get the memory out of the state
getMem :: M Mem
getMem = mem <$> get
-- | write the memory back to the state
putMem :: Mem -> M ()
putMem m = do c <- get; put c { mem = m }
-- | add some output
addOutput :: String -> M ()
addOutput s = do c <- get; put c { output = s : output c }
-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: (Env -> Env) -> M a -> M a
locally modifyEnv action = do 
  e <- getEnv
  putEnv $ modifyEnv e
  res <- action
  putEnv e
  return res where
  putEnv e = do c <- get; put c { env = e }

lTrue, lFalse :: Runtime
lTrue  = Number 1
lFalse = Number 0
defaultHeapSize :: Int64
defaultHeapSize = 1024 * 16 -- with 64 bit ints, i think this is one meg.
emptyMem :: Int64 -> IO Mem
emptyMem heapSize = do
  mem <- IOArray.newArray (0 :: Int64, heapSize - 1) (Number (0 :: Int64))
  return (mem, 0 :: Int64)

-- TODO: change interp in L.Compiler to be an m (Output) or IO (Output)
interpL3 :: L3 -> String
interpL3 = mkString "" . reverse . unsafePerformIO . getL3Output

getL3Output :: L3 -> IO Output
getL3Output p = output . snd <$> runInterpL3 defaultHeapSize p

-- | top level entry point (interprets e, and runs the monadic action)
evalL3 :: L3 -> IO Runtime
evalL3 p = fst <$> runInterpL3 defaultHeapSize p

mkComputer :: L3 -> Mem -> L3Computer
mkComputer (L3 _ fs) mem = L3Computer {
  env    = Map.empty
 ,mem    = mem
 ,output = []
 ,lib    = Map.fromList $ fmap (\f -> (name f, f)) fs --(zip fs $ fmap (*2) [0..])
}

runInterpL3 :: Int64 -> L3 -> IO (Runtime, L3Computer)
runInterpL3 heapSize p@(L3 e _) = do
  mem <- emptyMem heapSize
  runStateT (interpE e) $ mkComputer p mem

-- | interpret an E, building a monadic operation to be run.
interpE :: E -> M Runtime
interpE (Let v d e) = do
  d' <- interpD d
  locally (Map.insert v d') (interpE e)
interpE (IfStatement v te fe) = do
  v' <- interpV v
  interpE $ if v' == lTrue then te else fe
interpE (DE d) = interpD d

interpD :: D -> M Runtime
interpD (BiopD b l r) = 
  do l' <- interpV l; r' <- interpV r; return $ mathOp l' b r'
interpD (PredD IsNum   (NumV _)) = return $ lTrue
-- labels act like arrays, it's weird, but true.
interpD (PredD IsNum   _       ) = return $ lFalse
interpD (PredD IsArray (NumV _)) = return $ lFalse
interpD (PredD IsArray _       ) = return $ lTrue
interpD (NewArray s d)           = newArray s d
interpD (FunCall v vs)           = error "todo"
interpD (NewTuple vs)            = error "todo"
interpD (ARef a loc)             = error "todo"
interpD (ASet a loc v)           = error "todo"
interpD (ALen a)                 = error "todo"
interpD (L.L3.L3AST.Print v)     = error "todo"
interpD (MakeClosure l v)        = error "todo"
interpD (ClosureProc v)          = error "todo"
interpD (ClosureVars v)          = error "todo"
interpD (VD v)                   = error "todo"

newArray :: V -> V -> M Runtime
newArray s v = do
  size <- evalNumber s 
  val  <- interpV v
  makeHeapArray size $ replicate (fromIntegral size) val

-- makes a new array from the given Runtime objects and puts it on the heap
makeHeapArray :: Int64 -> [Runtime] -> M Runtime
makeHeapArray size rs = do
  (mem, hp) <- getMem
  _         <- lift $ writeArrayIntoHeap mem hp (Pointer hp : rs)
  _         <- putMem (mem, hp + size)
  return $ Pointer hp where
  writeArrayIntoHeap :: IOArray Int64 Runtime -> Int64 -> [Runtime] -> IO [()]
  writeArrayIntoHeap mem hp rs =
    Data.Traversable.sequence $ uncurry (IOArray.writeArray mem) <$> zip [hp..] rs

evalNumber, evalPointer  :: V -> M Int64
evalNumber  v = foldRuntime (\(Number i) -> i) evalErrN evalErrN <$> interpV v
  where evalErrN = evalErr "Number"
evalPointer v = foldRuntime evalErrP (\(Pointer i) -> i) evalErrP <$> interpV v
  where evalErrP = evalErr "Pointer"
--evalClosure :: E -> M Runtime
--evalClosure e = foldRuntime evalErrC evalErrC id <$> interp e
--  where evalErrC = evalErr "<function>"

evalErr :: String -> Runtime -> a -- hmm...
evalErr typ r = runtimeError $ "expected " ++ typ ++ " but got: " ++ (show r)

runtimeError :: String -> a
runtimeError msg = error $ "Runtime error: " ++ msg

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc r = f r where
  f n@(Number _)          = fn n
  f p@(Pointer _)         = fp p
  f c@(FunctionPointer _) = fc c

mathOp :: Runtime -> Biop -> Runtime -> Runtime  
mathOp (Number l) Add      (Number r) = Number    $ l +  r
mathOp (Number l) Sub      (Number r) = Number    $ l -  r
mathOp (Number l) Mult     (Number r) = Number    $ l *  r
mathOp (Number l) LessThan (Number r) = boolToNum $ l <  r
mathOp (Number l) LTorEq   (Number r) = boolToNum $ l <= r
mathOp (Number l) Eq       (Number r) = boolToNum $ l == r
mathOp l b r = 
  error $ concat ["invalid arguments to ", show b, " :", show l, ", " , show r]

boolToNum :: Bool -> Runtime
boolToNum True  = Number 1
boolToNum False = Number 0

interpV :: V -> M Runtime
interpV (VarV v) = do
  env <- getEnv
  return $ fromMaybe (error $ "unbound variable: " ++ v) (Map.lookup v $ traceShowId env)
interpV (NumV i) = return $ Number i
interpV (LabelV l) = error "todo" -- uh oh
