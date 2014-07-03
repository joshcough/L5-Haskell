module L.L5.L5Interp
  (
    interp
   ,runInterp
   ,locally
   ,letrecExample
  )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Traversable
import L.L5.L5AST
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray

data Runtime = Num Int | Pointer Int | Closure [Variable] E Env
  deriving Eq
instance Show Runtime where
  show (Num i) = show i
  show (Pointer _) = "pointer"
  show (Closure _ _ _) = "<function>"
type Mem = (IOArray Int Runtime, Int) -- heap, and heap pointer
type Env = Map Variable Runtime
type M x = StateT (Env, Mem) IO x

-- state functions
-- | get the memory out of the state
getMem = snd <$> get
-- | get the env out of the state
getEnv = fst <$> get
-- | write the memory back to the state
putMem m = do (e, _) <- get; put (e, m)
-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: (Env -> Env) -> M a -> M a
locally modifyEnv action = do 
  e <- getEnv
  putEnv $ modifyEnv e
  res <- action
  putEnv e
  return res where
  putEnv e = do (_, m) <- get; put (e, m)

heapSize = 1000
emptyMem :: IO Mem
emptyMem = do
  mem <- IOArray.newArray (0,heapSize - 1) (Num 0)
  return (mem, 0)

-- | top level entry point (interprets e, and runs the monadic action)
runInterp :: E -> IO Runtime
runInterp e = do
  mem <- emptyMem
  evalStateT (interp e) (Map.empty, mem)

-- | interpret an E, building a monadic operation to be run.
interp :: E -> M Runtime
interp (Lambda vs e)       = Closure vs e <$> getEnv
interp (Var v)             = envLookup  v <$> getEnv
interp (Let ves body)      = interp $ App (Lambda (fst <$> ves) body) (snd <$> ves) 
interp (LetRec ves body)   = locally newEnv (interp body) where
  newEnv old = new where new = Map.union (Map.fromList (toClosure <$> ves)) old 
                         toClosure (v, Lambda vs e) = (v, Closure vs e new)
interp (IfStatement p t f) = do r <- interp p; interp $ if r == lTrue then t else f
interp (NewTuple es)       = traverse interp es >>= makeHeapArray (length es)
interp (Begin e1 e2)       = interp e1 >> interp e2
interp (LitInt i)          = return $ Num i
interp (PrimFunE p)        = interpPrim p
interp (App f es)          = interpApp f es

-- | interpret a Primitive function
interpPrim :: PrimFun -> M Runtime
interpPrim (Print e)        = interpPrint e
interpPrim (Add e1 e2)      = binOp (+) e1 e2
interpPrim (Sub e1 e2)      = binOp (-) e1 e2
interpPrim (Mult e1 e2)     = binOp (*) e1 e2
interpPrim (LessThan e1 e2) = boolBinOp (<) e1 e2
interpPrim (LessThanOrEqualTo e1 e2) = boolBinOp (<=) e1 e2
interpPrim (EqualTo e1 e2)  = boolBinOp (==) e1 e2
interpPrim (IsNumber e)     = isNumber <$> interp e
interpPrim (IsArray e)      = isArray  <$> interp e
interpPrim (NewArray e1 e2) = newArray e1 e2
interpPrim (ARef e1 e2)     = arrayRef e1 e2
interpPrim (ASet e1 e2 e3)  = arraySet e1 e2 e3
interpPrim (ALen e)         = arrayLength e

-- | prints the given E.
interpPrint :: E -> M Runtime
interpPrint e = do r <- interp e; (showRuntime r >>= lift . print) >> return lTrue

-- | function application
interpApp :: E -> [E] -> M Runtime
interpApp f es = do
  (Closure vars body env) <- evalClosure f
  rs <- traverse interp es
  locally (\_ -> Map.union (Map.fromList (zip vars rs)) env) (interp body)

-- | array reference
arrayRef :: E -> E -> M Runtime
arrayRef e1 e2 = do
  (mem,_) <- getMem
  p       <- evalPointer e1 
  index   <- evalNumber e2
  lift $ IOArray.readArray mem (p + index + 1)

-- | get array length
arrayLength :: E -> M Runtime
arrayLength e = do
  (mem,_) <- getMem
  p       <- evalPointer e
  lift $ IOArray.readArray mem p

-- | sets the arr[i] = e
arraySet :: E -> E -> E -> M Runtime
arraySet arr i e = do
  p        <- evalPointer arr
  index    <- evalNumber i
  r        <- interp e
  (mem,hp) <- getMem
  _        <- lift $ IOArray.writeArray mem (p + index + 1) r
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

-- TODO: might be a function in Maybe to make this easier.
envLookup :: Variable -> Env -> Runtime
envLookup v env = maybe (runtimeError $ "unbound variable: " ++ v) id (Map.lookup v env)

evalNumber  :: E -> M Int
evalNumber  e = foldRuntime (\(Num i) -> i) evalErrN evalErrN <$> interp e 
  where evalErrN = evalErr "Number"
evalPointer :: E -> M Int
evalPointer e = foldRuntime evalErrP (\(Pointer i) -> i) evalErrP <$> interp e
  where evalErrP = evalErr "Pointer"
evalClosure :: E -> M Runtime
evalClosure e = foldRuntime evalErrC evalErrC id <$> interp e
  where evalErrC = evalErr "<function>"

evalErr :: String -> Runtime -> a -- hmm...
evalErr typ r = runtimeError $ "expected " ++ typ ++ " but got: " ++ (show r)

runtimeError :: String -> a
runtimeError msg = error $ "Runtime error: " ++ msg 

binOp :: (Int -> Int -> Int) -> E -> E -> M Runtime
binOp f e1 e2 = f <$> evalNumber e1 <*> evalNumber e2 >>= return . Num

-- I know 0 is horrible for true,
-- I didn't design this language.
boolBinOp :: (Int -> Int -> Bool) -> E -> E -> M Runtime
boolBinOp f = binOp $ \x y -> if f x y then 0 else 1 
 
getHeapList :: M [Runtime]
getHeapList = fst <$> getMem >>= lift . IOArray.getElems

showRuntime :: Runtime -> M String
showRuntime r = showRuntimePure <$> getHeapList <*> return r

showArray :: Int -> M String
showArray i = showArrayPure <$> getHeapList <*> return (Pointer i)

-- Pure Code

getArrayPure :: [Runtime] -> Int -> (Int, [Runtime])
getArrayPure mem i = (size, arr) where
  view       = drop i mem
  (Num size) = foldRuntime id err err $ head view
  err r      = runtimeError $ "bug, bad array index: " ++ show r
  arr        = take size $ drop 1 view

-- | True for the L5 programming language
lTrue  = Num 0
-- | False for L5, this cant be tested for equality, because everything other than 0 is false.
lFalse = Num 1 

isNumber :: Runtime -> Runtime
isNumber = foldRuntime (const lTrue) (const lFalse) (const lFalse)

isArray :: Runtime -> Runtime
isArray = foldRuntime (const lFalse) (const lTrue) (const lFalse)

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc r = f r where
  f n@(Num _)         = fn n
  f p@(Pointer _)     = fp p
  f c@(Closure _ _ _) = fc c

showRuntimePure :: [Runtime] -> Runtime -> String
showRuntimePure m = foldRuntime show (showArrayPure m) show

showArrayPure :: [Runtime] -> Runtime -> String
showArrayPure mem (Pointer i) = "{s:" ++ show size ++ "," ++ body ++ "}" where
  (size, arr) = getArrayPure mem i 
  body = join $ intersperse "," (show <$> arr)

zero = LitInt 0
one = LitInt 1
f = Var "f"
g = Var "g"
x = Var "x"
lambdaExample rec = Lambda ["x"] ifExample where
  beginExample = Begin (PrimFunE $ Print x) (App rec [PrimFunE $ Sub x one])
  ifExample = IfStatement (PrimFunE $ EqualTo x zero) x beginExample
letrecExample = LetRec [("f", lambdaExample g), ("g", lambdaExample f)] $ App f [LitInt 15]
