module L.L5.L5Interp where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable
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
type S = (Env, Mem)
type M = StateT S IO Runtime

-- state functions
getMem = snd <$> get
getEnv = fst <$> get
putEnv e = do (_, m) <- get; put (e, m)
putMem m = do (e, _) <- get; put (e, m)
locally modifyEnv action = do 
  e <- getEnv
  putEnv $ modifyEnv e
  res <- action
  putEnv e
  return res

runInterp :: E -> IO Runtime
runInterp e = evalStateT (interp e) (Map.empty, [])

interp :: E -> M
interp (Lambda vs e)       = do env <- getEnv; return $ (Closure vs e env)
interp (Var v)             = envLookup v <$> getEnv
interp (Let ves body)      = interp $ App (Lambda (fst <$> ves) body) (snd <$> ves) 
interp (LetRec v e body)   = error "todo"
interp (IfStatement p t f) = do pv <- interp p; interp $ case pv of (Num 0) -> t; _ -> f
interp (NewTuple es)       = traverse interp es >>= makeArray (length es)
interp (Begin e1 e2)       = do _ <- interp e1; interp e2
interp (LitInt i)          = return $ Num i
interp (PrimFunE p)        = interpPrim p
interp (App f es)          = interpApp f es

interpPrim :: PrimFun -> M
interpPrim (Print e)        = interpPrint e
interpPrim (Add e1 e2)      = binOp (+) e1 e2
interpPrim (Sub e1 e2)      = binOp (-) e1 e2
interpPrim (Mult e1 e2)     = binOp (*) e1 e2
interpPrim (LessThan e1 e2) = boolBinOp (<) e1 e2
interpPrim (LessThanOrEqualTo e1 e2) = boolBinOp (<=) e1 e2
interpPrim (EqualTo e1 e2)  = boolBinOp (==) e1 e2
interpPrim (IsNumber e)     = isNumber <$> interp e
interpPrim (IsArray e)      = isArray <$> interp e
interpPrim (NewArray e1 e2) = newArray e1 e2
interpPrim (ARef e1 e2)     = arrayRef e1 e2
interpPrim (ASet e1 e2 e3)  = arraySet e1 e2 e3
interpPrim (ALen e)         = arrayLength e

interpPrint :: E -> M
interpPrint e = do
  r <- interp e
  printRuntime r >> lift (print "\n") >> return lTrue

interpApp :: E -> [E] -> M
interpApp f es = do
  (Closure vars body env) <- evalClosure f
  rs <- traverse interp es
  locally (\_ -> Map.union (Map.fromList (zip vars rs)) env) (interp body)

-- TODO: this is obviously horribly inefficient and needs MArray or STArray
makeArray :: Int -> [Runtime] -> M
makeArray size rs = do
  mem <- getMem
  _   <- putMem $ mem ++ [Num size] ++ rs
  return $ Num $ length mem

-- TODO: might be a function in Maybe to make this easier.
envLookup :: Variable -> Env -> Runtime
envLookup v env = maybe (error $ "unbound variable: " ++ v) id (Map.lookup v env)

newArray e1 e2 = do
  (Num size) <- evalNumber e1
  val        <- interp e2
  makeArray size $ replicate size val

arrayRef e1 e2 = do
  (Pointer p) <- evalPointer e1
  (Num index) <- evalNumber e2
  mem         <- getMem
  return $ mem !! (p + index + 1)

arrayLength e = do
  (Pointer index) <- evalPointer e
  mem <- getMem
  return . arraySize $ mem !! index

arraySet e1 e2 e3 = do
  (Pointer p) <- evalPointer e1
  (Num index) <- evalNumber e2
  r           <- interp e3
  mem         <- getMem
  _           <- putMem $ toList $ Seq.update (p + index + 1) r (Seq.fromList mem)
  return lTrue
  
lTrue  = Num 0 -- this is the only true value
lFalse = Num 1 -- this cant be tested for equality, because everything other than 0 is false.

isTrue :: Runtime -> Runtime
isTrue r = if r == lTrue then lTrue else lFalse

isNumber :: Runtime -> Runtime
isNumber = foldRuntime (const lTrue) (const lFalse) (const lFalse)

isArray :: Runtime -> Runtime
isArray = foldRuntime (const lFalse) (const lTrue) (const lFalse)

evalNumber  :: E -> M
evalNumber  e = foldRuntime id evalErrN evalErrN <$> interp e 
  where evalErrN = evalErr "Number"
evalPointer :: E -> M
evalPointer e = foldRuntime evalErrP id evalErrP <$> interp e
  where evalErrP = evalErr "Pointer"
evalClosure :: E -> M
evalClosure e = foldRuntime evalErrC evalErrC id <$> interp e
  where evalErrC = evalErr "<function>"

evalErr :: String -> Runtime -> a -- hmm...
evalErr typ r = error $ "expected " ++ typ ++ " but got: " ++ (show r)

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc r = f r where
  f n@(Num _)         = fn n
  f p@(Pointer _)     = fp p
  f c@(Closure _ _ _) = fc c

binOp :: (Int -> Int -> Int) -> E -> E -> M
binOp f e1 e2 = do
  r1 <- evalNumber e1
  r2 <- evalNumber e2
  return . Num $ f (getVal r1) (getVal r2) where
  getVal (Num i) = i

boolBinOp :: (Int -> Int -> Bool) -> E -> E -> M
boolBinOp f = binOp $ \x y -> boolToNum $ f x y where 
  -- I know 0 is horrible for true,
  -- I didn't design this language.
  boolToNum b = if b then 0 else 1
  
printRuntime :: Runtime -> StateT S IO ()
printRuntime (Num i)         = lift $ print i
printRuntime (Pointer i)     = printMem i
printRuntime (Closure _ _ _) = lift $ print "<function>"

printMem :: Int -> StateT S IO ()
printMem i = do
  mem <- getMem
  let view       = drop i mem
      (Num size) = arraySize $ head view 
      arr        = take size $ drop 1 view
  traverse_ printRuntime arr

arraySize :: Runtime -> Runtime
arraySize (Num s) = Num s
arraySize _ = error "bad array size"
