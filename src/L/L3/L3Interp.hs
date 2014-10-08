module L.L3.L3Interp (
   evalL3
  ,interpL3
  ,runInterpL3
) where

import Control.Applicative
import Control.Monad.State
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Debug.Trace
import L.L1L2AST hiding (Func)
import L.L3.L3AST
import L.Utils
import System.IO.Unsafe

data Runtime = Num Int64 | Pointer Int64 | FunctionPointer Label deriving Eq
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
  e   <- getEnv
  putEnv $ modifyEnv e
  res <- action
  putEnv e
  return res where putEnv e = do c <- get; put c { env = e }

lTrue, lFalse :: Runtime
lTrue  = Num 1
lFalse = Num 0
defaultHeapSize :: Int64
defaultHeapSize = 1024 * 16 -- with 64 bit ints, i think this is one meg.
emptyMem :: Int64 -> IO Mem
emptyMem heapSize = do
  mem <- IOArray.newArray (0 :: Int64, heapSize - 1) (Num (0 :: Int64))
  return (mem, 0 :: Int64)

-- TODO: change interp in L.Compiler to be an m (Output) or IO (Output)
interpL3 :: L3 -> String
interpL3 p = unsafePerformIO $ do
  o <- getL3Output p
  return $ mkString "" $ reverse o

interpL3_ :: L3 -> String
interpL3_ p = unsafePerformIO $ do
  (_, c) <- runInterpL3 defaultHeapSize p
  showMem (mem c)

showMem :: (IOArray Int64 Runtime, Int64) -> IO String
showMem (arr, hp) = do
  arrS <- showArr arr
  return $ "heapPointer: " ++ show hp ++ " arr: " ++ arrS

showArr :: IOArray Int64 Runtime -> IO String
showArr arr = do
  rs <- IOArray.getElems arr
  return $ mkString ", " $ fmap showRuntimeSimple rs

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
interpD (PredD IsNum (NumV _  )) = return $ lTrue
interpD (PredD IsNum v@(VarV _)) =
  do rv <- interpV v; return $ case rv of (Num _) -> lTrue; _ -> lFalse
-- labels act like arrays, it's weird, but true.
interpD (PredD IsNum   _       ) = return $ lFalse
interpD (PredD IsArray v@(VarV _)) =
  do rv <- interpV v; return $ case rv of (Pointer _) -> lTrue; _ -> lFalse
interpD (PredD IsArray _       ) = return $ lTrue
interpD (NewArray s d)           = newArray s d
interpD (FunCall v vs)           = interpApp v vs
interpD (NewTuple vs)            =
  traverse interpV vs >>= makeHeapArray (fromIntegral $ length vs)
interpD (ARef a loc)             = arrayRef a loc
interpD (ASet a loc v)           = arraySet a loc v
interpD (ALen a)                 = arrayLength a
interpD (L.L3.L3AST.Print v)     = interpPrint v
interpD (MakeClosure l v)        = interpD (NewTuple [LabelV l, v])
interpD (ClosureProc c)          = arrayRef c (NumV 0)
interpD (ClosureVars c)          = arrayRef c (NumV 1)
interpD (VD v)                   = interpV v

interpV :: V -> M Runtime
interpV (VarV v)   = envLookup v <$> getEnv
interpV (NumV i)   = return $ Num i
interpV (LabelV l) = return $ FunctionPointer l

newArray :: V -> V -> M Runtime
newArray s v = do
  size <- evalNumber s
  val  <- interpV v
  makeHeapArray size $ replicate (fromIntegral size) val

-- makes a new array from the given Runtime objects and puts it on the heap
makeHeapArray :: Int64 -> [Runtime] -> M Runtime
makeHeapArray size rs = do
  (mem, hp) <- getMem
  _         <- lift $ writeArrayIntoHeap mem hp (Num size : rs)
  _         <- putMem (mem, hp + size + 1)
  return $ Pointer hp where
  writeArrayIntoHeap :: IOArray Int64 Runtime -> Int64 -> [Runtime] -> IO [()]
  writeArrayIntoHeap mem hp rs =
    Data.Traversable.sequence $ uncurry (IOArray.writeArray mem) <$> zip [hp..] rs

-- | prints the given E. (print e)
interpPrint :: V -> M Runtime
interpPrint v = do
  r <- interpV v
  s <- showRuntime r
  _ <- addOutput (s ++ "\n")
  return lFalse

-- | function application (f v...)
interpApp :: V -> [V] -> M Runtime
interpApp f vs = do
  (FunctionPointer label) <- interpV f
  rs                      <- traverse interpV vs
  (Func _ args body)      <- libLookup label <$> getLib
  env                     <- getEnv
  locally (\_ -> Map.union (Map.fromList $ zip args rs) env) (interpE body)

-- | array reference (arr[i])
arrayRef :: V -> V -> M Runtime
arrayRef arr i = do
  (mem,_) <- getMem
  p       <- evalPointer arr
  index   <- evalNumber i
  lift $ IOArray.readArray mem (p + index + 1)

-- | get array length (size arr)
arrayLength :: V -> M Runtime
arrayLength arr = do
  (mem,_) <- getMem
  p       <- evalPointer arr
  lift $ IOArray.readArray mem p

-- | sets the (arr[i] = e)
arraySet :: V -> V -> V -> M Runtime
arraySet arr i v = do
  p        <- evalPointer arr
  index    <- evalNumber i
  r        <- interpV v
  (mem,_)  <- getMem
  ()       <- lift $ IOArray.writeArray mem (p + index + 1) r
  return lTrue


evalNumber, evalPointer  :: V -> M Int64
evalNumber  v = foldRuntime (\(Num i) -> i) evalErrN evalErrN <$> interpV v
  where evalErrN = evalErr "Num"
evalPointer v = foldRuntime evalErrP (\(Pointer i) -> i) evalErrP <$> interpV v
  where evalErrP = evalErr "Pointer"
--evalClosure :: E -> M Runtime
--evalClosure e = foldRuntime evalErrC evalErrC id <$> interp e
--  where evalErrC = evalErr "<function>"

evalErr :: String -> Runtime -> a -- hmm...
evalErr typ r = runtimeError $ "expected " ++ typ ++ " but got: " ++ showRuntimeSimple r

showRuntimeSimple :: Runtime -> String
showRuntimeSimple (Num n)     = "Num "     ++ show n
showRuntimeSimple (Pointer n) = "Pointer " ++ show n
showRuntimeSimple (FunctionPointer l) = "FunctionPointer " ++ l

runtimeError :: String -> a
runtimeError msg = error $ "Runtime error: " ++ msg

getHeapList :: M [Runtime]
getHeapList = fst <$> getMem >>= lift . IOArray.getElems

showRuntime :: Runtime -> M String
showRuntime r = showRuntimePure <$> getHeapList <*> return r

envLookup :: Variable -> Env -> Runtime
envLookup = forceLookup "variable"

libLookup :: Variable -> Lib -> Func
libLookup = forceLookup "function"

forceLookup :: (Show k, Ord k) => String -> k -> Map k v -> v
forceLookup kName k m =
  fromMaybe (runtimeError $ concat ["unbound ", kName, ": ", show k]) (Map.lookup k m)

mathOp :: Runtime -> Biop -> Runtime -> Runtime  
mathOp (Num l) Add      (Num r) = Num    $ l +  r
mathOp (Num l) Sub      (Num r) = Num    $ l -  r
mathOp (Num l) Mult     (Num r) = Num    $ l *  r
mathOp (Num l) LessThan (Num r) = boolToNum $ l <  r
mathOp (Num l) LTorEq   (Num r) = boolToNum $ l <= r
mathOp (Num l) Eq       (Num r) = boolToNum $ l == r
mathOp l b r = error $
  concat ["invalid arguments to ", show b, " :", showRuntimeSimple l, ", " , showRuntimeSimple r]

boolToNum :: Bool -> Runtime
boolToNum True  = Num 1
boolToNum False = Num 0

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc r = f r where
  f n@(Num _)             = fn n
  f p@(Pointer _)         = fp p
  f c@(FunctionPointer _) = fc c

showRuntimePure :: [Runtime] -> Runtime -> String
showRuntimePure m = foldRuntime
 (\(Num n) -> show n) (showArrayPure m) (\l -> runtimeError $ "can't print a label: " ++ showRuntimeSimple l)

getArrayPure :: [Runtime] -> Int64 -> (Int64, [Runtime])
getArrayPure mem i = (size, arr) where
  view       = drop (fromIntegral i) mem
  (Num size) = foldRuntime id err err $ head view
  err r      = runtimeError $ "bug, bad array size: " ++ showRuntimeSimple r
  arr        = take (fromIntegral size) $ drop 1 view

showArrayPure :: [Runtime] -> Runtime -> String
showArrayPure mem (Pointer i) = concat ["{s:", show size, ", ", body, "}"] where
  (size, arr) = getArrayPure mem i
  body = join $ intersperse ", " (showRuntimePure mem <$> arr)
showArrayPure _ e = error $ "impossible (showArrayPure called with: " ++ showRuntimeSimple e ++ ")"
