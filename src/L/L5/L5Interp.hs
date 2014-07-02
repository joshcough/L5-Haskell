module L.L5.L5Interp
  (
    interp
   ,runInterp
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

data Runtime = Num Int | Pointer Int | Closure [Variable] E Env
  deriving Eq
instance Show Runtime where
  show (Num i) = show i
  show (Pointer _) = "pointer"
  show (Closure _ _ _) = "<function>"
type Mem = [Runtime]
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
locally modifyEnv action = do 
  e <- getEnv
  putEnv $ modifyEnv e
  res <- action
  putEnv e
  return res where
  putEnv e = do (_, m) <- get; put (e, m)

-- | top level entry point (interprets e, and runs the monadic action)
runInterp :: E -> IO Runtime
runInterp e = evalStateT (interp e) (Map.empty, [])

-- | interpret an E, building a monadic operation to be run.
interp :: E -> M Runtime
interp (Lambda vs e)       = Closure vs e <$> getEnv
interp (Var v)             = envLookup  v <$> getEnv
interp (Let ves body)      = interp $ App (Lambda (fst <$> ves) body) (snd <$> ves) 
interp (LetRec v e body)   = error "todo"
interp (IfStatement p t f) = do r <- interp p; interp $ if r == lTrue then t else f
interp (NewTuple es)       = traverse interp es >>= makeArray (length es)
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
  (_, arr) <- evalArray e1
  index    <- evalNumber e2
  return $ arr !! index

-- | get array length
arrayLength :: E -> M Runtime
arrayLength e = Num . fst <$> evalArray e

-- TODO: can this be rewritten with evalArray after changing to STArray?
arraySet :: E -> E -> E -> M Runtime
arraySet e1 e2 e3 = do
  p     <- evalPointer e1
  index <- evalNumber e2
  r     <- interp e3
  mem   <- getMem
  _     <- putMem $ toList $ Seq.update (p + index + 1) r (Seq.fromList mem)
  return lTrue

-- makes a new array from the given Runtime objects
makeArray :: Int -> [Runtime] -> M Runtime
makeArray size rs = do
  (pointer, newMem) <- makeArrayPure <$> getMem <*> return size <*> return rs
  _ <- putMem $ newMem
  return $ pointer

-- makes an array of size e1 with each slot filled with e2
newArray :: E -> E -> M Runtime
newArray e1 e2 = do
  size <- evalNumber e1
  val  <- interp e2
  makeArray size $ replicate size val

-- TODO: might be a function in Maybe to make this easier.
envLookup :: Variable -> Env -> Runtime
envLookup v env = maybe (runtimeError $ "unbound variable: " ++ v) id (Map.lookup v env)

getArray :: Int -> M (Int, [Runtime])
getArray i = getArrayPure <$> getMem <*> return i

evalNumber  :: E -> M Int
evalNumber  e = foldRuntime (\(Num i) -> i) evalErrN evalErrN <$> interp e 
  where evalErrN = evalErr "Number"
evalPointer :: E -> M Int
evalPointer e = foldRuntime evalErrP (\(Pointer i) -> i) evalErrP <$> interp e
  where evalErrP = evalErr "Pointer"
evalClosure :: E -> M Runtime
evalClosure e = foldRuntime evalErrC evalErrC id <$> interp e
  where evalErrC = evalErr "<function>"
evalArray :: E -> M (Int, [Runtime])
evalArray e = evalPointer e >>= getArray

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
  
showRuntime :: Runtime -> M String
showRuntime r = showRuntimePure <$> getMem <*> return r

showArray :: Int -> M String
showArray i = showArrayPure <$> getMem <*> return (Pointer i)

-- Pure Code

-- TODO: this is obviously horribly inefficient and needs MArray or STArray
-- returns a pointer to the array in the heap
-- and the new heap
makeArrayPure :: Mem -> Int -> [Runtime] -> (Runtime, Mem)
makeArrayPure mem size rs = (Pointer $ length mem, mem ++ [Num size] ++ rs)

getArrayPure :: Mem -> Int -> (Int, [Runtime])
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

showRuntimePure :: Mem -> Runtime -> String
showRuntimePure m = foldRuntime show (showArrayPure m) show

showArrayPure :: Mem -> Runtime -> String
showArrayPure mem (Pointer i) = "{s:" ++ show size ++ "," ++ body ++ "}" where
  (size, arr) = getArrayPure mem i 
  body = join $ intersperse "," (show <$> arr)
