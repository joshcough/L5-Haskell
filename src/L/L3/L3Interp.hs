module L.L3.L3Interp (runInterpL3) where

import Control.Applicative
import Control.Monad.State
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Data.Tuple.HT
import L.L1L2AST hiding (Func)
import L.L3.L3AST
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray

data Runtime = 
    Number Int64
  | Pointer Int64
  | FunctionPointer Label deriving (Show, Eq)
type Output = [String]
type Mem = (IOArray Int64 Runtime, Int64) -- heap, and heap pointer
type Env = Map Variable Runtime
type Lib = Map Label Func
type M x = StateT ((Env, Mem, Output), Lib) IO x

-- state functions
-- | get the env out of the state
getEnv :: M Env
getEnv = fst3 . fst <$> get
-- | get the library out of the state
getLib :: M Lib
getLib = snd <$> get
-- | get the memory out of the state
getMem :: M Mem
getMem = snd3 . fst <$> get
-- | write the memory back to the state
putMem :: Mem -> M ()
putMem m = do ((e, _, o), l) <- get; put ((e, m, o), l)
-- | add some output
addOutput :: String -> M ()
addOutput s = do ((e, m, o), l) <- get; put ((e, m, s:o), l)
-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: (Env -> Env) -> M a -> M a
locally modifyEnv action = do 
  e <- getEnv
  putEnv $ modifyEnv e
  res <- action
  putEnv e
  return res where
  putEnv e = do ((_, m, o), l) <- get; put ((e, m, o), l)

lTrue, lFalse :: Runtime
lTrue  = Number 1
lFalse = Number 0
defaultHeapSize :: Int64
defaultHeapSize = 1024 * 16 -- with 64 bit ints, i think this is one meg.
emptyMem :: Int64 -> IO Mem
emptyMem heapSize = do
  mem <- IOArray.newArray (0 :: Int64, heapSize - 1) (Number (0 :: Int64))
  return (mem, 0 :: Int64)

-- | top level entry point (interprets e, and runs the monadic action)
runInterpL3 :: L3 -> IO Runtime
runInterpL3 = runInterpL3' defaultHeapSize 

runInterpL3' :: Int64 -> L3 -> IO Runtime
runInterpL3' heapSize (L3 e fs) = do
  mem <- emptyMem heapSize
  let lib = Map.fromList $ fmap (\f -> (name f, f)) fs--(zip fs $ fmap (*2) [0..])
  evalStateT (interpE e) ((Map.empty, mem, []),lib)

-- | interpret an E, building a monadic operation to be run.
interpE :: E -> M Runtime
interpE (Let v d e) = do
  d' <- interpD d
  locally (Map.insert v d') (interpE e)
interpE (IfStatement v te fe) = do
  v' <- interpV v
  interpE $ if v' == lTrue then te else fe
interpE (DE d) = interpD d

{-
data D    =
    FunCall V [V]
  | NewTuple [V]
  | ARef V V
  | ASet V V V
  | ALen V
  | Print V
  | MakeClosure Label V
  | ClosureProc V
  | ClosureVar
-}
interpD :: D -> M Runtime
interpD (BiopD b l r) = 
  do l' <- interpV l; r' <- interpV r; return $ mathOp l' b r'
interpD (PredD IsNum   (NumV _)) = return $ lTrue
-- labels act like arrays, it's weird, but true.
interpD (PredD IsNum   _       ) = return $ lFalse
interpD (PredD IsArray (NumV _)) = return $ lFalse
interpD (PredD IsArray _       ) = return $ lTrue
interpD (NewArray s d) = newArray s d
  

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
  return $ fromMaybe (error $ "unbound variable: " ++ v) (Map.lookup v env)
interpV (NumV i) = return $ Number i
interpV (LabelV l) = error "todo" -- uh oh

  {-
interp (Lambda vs e)       = Closure vs e <$> getEnv
interp (Var v)             = envLookup  v <$> getEnv
interp (Let ves body)      = interp $ App (Lambda (fst <$> ves) body) (snd <$> ves) 

interp (LetRec ves body)   = locally newEnv (interp body) where
  newEnv old = new where new = Map.union (Map.fromList (toClosure <$> ves)) old 
                         toClosure (v, Lambda vs e) = (v, Closure vs e new)
interp (IfStatement p t f) = do r <- interp p; interp $ if r == lTrue then t else f
interp (NewTuple es)       = traverse interp es >>= makeHeapArray (length es)
interp (Begin e1 e2)       = interp e1 >> interp e2
interp (LitInt i)          = return $ Number i
interp (PrimFunE p)        = interpPrim p
interp (App f es)          = interpApp f es

-- | interpret a Primitive function
interpPrim :: PrimFun -> M Runtime
interpPrim (Print    e)        = interpPrint e
interpPrim (Add      e1 e2)    = intBinOp  (+) e1 e2
interpPrim (Sub      e1 e2)    = intBinOp  (-) e1 e2
interpPrim (Mult     e1 e2)    = intBinOp  (*) e1 e2
interpPrim (LessThan e1 e2)    = boolBinOp (<) e1 e2
interpPrim (LTorEQ   e1 e2)    = boolBinOp (<=) e1 e2
interpPrim (EqualTo  e1 e2)    = boolBinOp (==) e1 e2
interpPrim (IsNumber e)        = isNumber <$> interp e
interpPrim (IsArray  e)        = isArray  <$> interp e
interpPrim (NewArray e1 e2)    = newArray e1 e2
interpPrim (ARef     e1 e2)    = arrayRef e1 e2
interpPrim (ASet     e1 e2 e3) = arraySet e1 e2 e3
interpPrim (ALen     e)        = arrayLength e

-- | prints the given E. (print e)
interpPrint :: E -> M Runtime
interpPrint e = do r <- interp e; (showRuntime r >>= lift . print) >> return lTrue

-- | function application (f e...)
interpApp :: E -> [E] -> M Runtime
interpApp f es = do
  (Closure vars body env) <- evalClosure f
  rs <- traverse interp es
  locally (\_ -> Map.union (Map.fromList (zip vars rs)) env) (interp body)

-- | array reference (arr[i])
arrayRef :: E -> E -> M Runtime
arrayRef arr i = do
  (mem,_) <- getMem
  p       <- evalPointer arr
  index   <- evalNumber i
  lift $ IOArray.readArray mem (p + index + 1)

-- | get array length (size arr)
arrayLength :: E -> M Runtime
arrayLength arr = do
  (mem,_) <- getMem
  p       <- evalPointer arr
  lift $ IOArray.readArray mem p

-- | sets the (arr[i] = e)
arraySet :: E -> E -> E -> M Runtime
arraySet arr i e = do
  p        <- evalPointer arr
  index    <- evalNumber i
  r        <- interp e
  (mem,_)  <- getMem
  ()       <- lift $ IOArray.writeArray mem (p + index + 1) r
  return lTrue

-- makes a new array from the given Runtime objects and puts it on the heap
makeHeapArray :: Int -> [Runtime] -> M Runtime
makeHeapArray size rs = do
  (mem, hp) <- getMem
  _         <- lift $ writeArrayIntoHeap mem hp (Pointer hp : rs)
  _         <- putMem (mem, hp + size)
  return $ Pointer hp where
  writeArrayIntoHeap :: IOArray Int Runtime -> Int -> [Runtime] -> IO [()]
  writeArrayIntoHeap mem hp rs = 
    Data.Traversable.sequence $ uncurry (IOArray.writeArray mem) <$> zip [hp..] rs 

-- makes an array of size e1 with each slot filled with e2
newArray :: E -> E -> M Runtime
newArray e1 e2 = do
  size <- evalNumber e1
  val  <- interp e2
  makeHeapArray size $ replicate size val

evalNumber, evalPointer  :: E -> M Int
evalNumber  e = foldRuntime (\(Number i) -> i) evalErrN evalErrN <$> interp e 
  where evalErrN = evalErr "Number"
evalPointer e = foldRuntime evalErrP (\(Pointer i) -> i) evalErrP <$> interp e
  where evalErrP = evalErr "Pointer"
evalClosure :: E -> M Runtime
evalClosure e = foldRuntime evalErrC evalErrC id <$> interp e
  where evalErrC = evalErr "<function>"

evalErr :: String -> Runtime -> a -- hmm...
evalErr typ r = runtimeError $ "expected " ++ typ ++ " but got: " ++ (show r)

runtimeError :: String -> a
runtimeError msg = error $ "Runtime error: " ++ msg 

intBinOp :: (Int -> Int -> Int) -> E -> E -> M Runtime
intBinOp f e1 e2 = f <$> evalNumber e1 <*> evalNumber e2 >>= return . Num

-- I know 0 is horrible for true,
-- I didn't design this language.
boolBinOp :: (Int -> Int -> Bool) -> E -> E -> M Runtime
boolBinOp f = intBinOp $ \x y -> if f x y then 0 else 1
 
getHeapList :: M [Runtime]
getHeapList = fst <$> getMem >>= lift . IOArray.getElems

showRuntime :: Runtime -> M String
showRuntime r = showRuntimePure <$> getHeapList <*> return r

showArray :: Int -> M String
showArray i = showArrayPure <$> getHeapList <*> return (Pointer i)

-- Pure Code

envLookup :: Variable -> Env -> Runtime
envLookup v env =
  fromMaybe (runtimeError $ "unbound variable: " ++ v) (Map.lookup v env)

getArrayPure :: [Runtime] -> Int -> (Int, [Runtime])
getArrayPure mem i = (size, arr) where
  view       = drop i mem
  (Number size) = foldRuntime id err err $ head view
  err r      = runtimeError $ "bug, bad array index: " ++ show r
  arr        = take size $ drop 1 view

-- | True for the L5 programming language
lTrue  = Number 0
-- | False for L5, this cant be tested for equality, because everything other than 0 is false.
lFalse = Number 1 

isNumber, isArray :: Runtime -> Runtime
isNumber = foldRuntime (const lTrue) (const lFalse) (const lFalse)
isArray  = foldRuntime (const lFalse) (const lTrue) (const lFalse)

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc r = f r where
  f n@(Number _)         = fn n
  f p@(Pointer _)     = fp p
  f c@(Closure _ _ _) = fc c

showRuntimePure :: [Runtime] -> Runtime -> String
showRuntimePure m = foldRuntime show (showArrayPure m) show

showArrayPure :: [Runtime] -> Runtime -> String
showArrayPure mem (Pointer i) = "{s:" ++ show size ++ "," ++ body ++ "}" where
  (size, arr) = getArrayPure mem i 
  body = join $ intersperse "," (show <$> arr)
-}
