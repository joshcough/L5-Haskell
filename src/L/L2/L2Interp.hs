module L.L2.L2Interp (interpL2) where

import Control.Applicative
import Control.Lens hiding (cons, set)
import Control.Monad.State
import Data.Int
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as Vector
import Debug.Trace
import L.Computer
import L.L1L2AST 
import L.L1L2MainAdjuster 
import Prelude hiding (head, print, tail)

interpL2 :: L2 -> String
interpL2 = showComputerOutput . interpL2'

-- run the given L2 program to completion on a new computer
-- return the computer as the final result.
interpL2' :: L2 -> Computer L2Instruction
interpL2' p = fst $ runState (runComputerM step c) emptyEnvs where
  c = newComputer $ adjustMain p

type M x = State (NonEmpty Env) x

-- Env, and some Env operations
type Env = Map Variable Int64
emptyEnvs :: NonEmpty Env
emptyEnvs = Map.empty :| []
replaceHeadEnv :: Env -> M ()
replaceHeadEnv e = do es <- get; put $ e :| tail es
addEnv :: Env -> M ()
addEnv e = do es <- get; put $ cons e es

step :: Computer L2Instruction -> M (Computer L2Instruction)
step c = let
 readS :: L2S -> M Int64
 readS (NumberL2S n) = return n
 readS (XL2S x)      = readX x
 readS (LabelL2S l)  = return $ findLabelIndex l c

 readX :: L2X -> M Int64
 readX (RegL2X r) = return $ readReg r c
 readX (VarL2X v) = 
   do e <- head <$> get; return $ fromMaybe (unbound v) (Map.lookup v e)

 debug f = do
   env <- get
   trace ("ip: " ++ show (c^.ip) ++ " inst: " ++ show (currentInst c) ++ " env:" ++ show env) f

 {- uncomment debug below to print ip and env for each instruction -}
 in {-debug $-} case currentInst c of

  -- Assignment statements
  (Assign x (CompRHS (Comp s1 op s2))) -> do
    s1' <- readS s1
    s2' <- readS s2
    nextInstWX x (if cmp op s1' s2' then 1 else 0) c
  (Assign x1 (MemRead (MemLoc x2 offset))) -> do
    index <- (fromIntegral offset +) <$> readX x2
    nextInstWX x1 (readMem "step MemRead" index c) c
  (Assign x (Allocate size datum)) -> do
    size'  <- readS size
    datum' <- readS datum
    let (h, c') = allocate size' datum' c
    nextInstWX x h c'
  (Assign x (Print s)) -> do
    s' <- readS s
    nextInstWX x 1 $ print s' c
  (Assign _ (ArrayError s1 s2)) ->
    arrayError <$> readS s1 <*> readS s2 <*> return c
  (Assign x (SRHS s)) -> do
    s' <- readS s
    nextInstWX x s' c
  -- Math Inst
  (MathInst x op s) -> do 
    x' <- readX x
    s' <- readS s
    nextInstWX x (runOp op x' s') c
  -- CJump
  (CJump (Comp s1 op s2) l1 l2) -> do
    s1' <- readS s1
    s2' <- readS s2
    return $ goto (findLabelIndex (if cmp op s1' s2' then l1 else l2) c) c
  -- MemWrite
  (MemWrite (MemLoc x offset) s) -> do
    index <- (fromIntegral offset +) <$> readX x
    s' <- readS s
    return . nextInst $ writeMem "step MemWrite" index s' c
  -- Goto
  (Goto l) -> return $ goto (findLabelIndex l c) c
  -- LabelDec, just advance
  (LabelDeclaration _) -> return $ nextInst c
  -- Call
  (Call s) -> do
    func <- readS s
    addEnv $ Map.empty
    let c' = push (c^.ip + 1) c
    return $ goto func c'
  -- TailCall
  (TailCall s) -> do
    loc <- readS s
    replaceHeadEnv Map.empty
    return $ goto loc c
  -- Return
  Return ->
    let rspVal = readReg rsp c
        done   = rspVal >= (fromIntegral $ Vector.length (c^.memory) * 8)
        c'     = writeReg rsp (rspVal + 8) c
        c''    = goto (readMem "step Return" rspVal c') c'
        f :: Bool -> [Env] -> (NonEmpty Env, Computer L2Instruction)
        -- in this case, we must be returning from main
        -- the computer thinks it's finished
        -- there are no environments left
        -- it's ok to halt the computer
        f True  [] = (Map.empty :| [], halt c) 
        -- in this case we are trying to return
        -- an we think the computer is finished, 
        -- but there are extra environments remaining
        -- there must be some programming error
        f True  xs = error $ "computer finished, but multiple environments exist" ++ show xs
        -- here we are trying to return
        -- and there are no more environments
        -- but the computer isnt in a finished state...
        -- there must be some programming error
        f False [] = error $ "trying to return, but no environments remaining"
        -- we are returning, and the computer is not halted
        -- based on the last case, there must be enough environments remaining
        -- were looking at the tail, so we've already popped the top env off
        -- so just return what we have
        f False (x:xs) = (x:|xs, c'')
    in do 
      es <- tail <$> get
      let (newEs,c) = f done es
      put newEs
      return c where

-- goto the next instruction after writing an x value
nextInstWX :: L2X -> Int64 -> Computer L2Instruction -> M (Computer L2Instruction)
nextInstWX x i c = nextInst <$> writeX x i where
  writeX :: L2X -> Int64 -> M (Computer L2Instruction)
  writeX (RegL2X r) i = return $ writeReg r i c
  writeX (VarL2X v) i = 
    do e <- head <$> get; replaceHeadEnv $ Map.insert v i e; return c

unbound :: Variable -> a
unbound v = error $ "unbound variable: " ++ v

