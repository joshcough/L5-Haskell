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
import L.Computer
import L.L1L2AST 
import Prelude hiding (head, print, tail)

interpL2 :: L2 -> String
interpL2 p = showComputerOutput $ interpL2' p

-- run the given L2 program to completion on a new computer
-- return the computer as the final result.
interpL2' :: L2 -> Computer L2Instruction
interpL2' p = error "todo" --runComputer step (newComputer p) 

{-
-- the main loop, runs a computer until completion
runComputer :: (a -> Computer a -> Computer a) -> Computer a -> Computer a
runComputer step c
  | c^.halted || not (hasNextInst c) = c
  | otherwise = runComputer step $ step (currentInst c) c
-}

type Env = Map Variable Int64
type M x = State (NonEmpty Env) x

step :: L2Instruction -> Computer L2Instruction -> M (Computer L2Instruction)
-- Assignment statements
step (Assign x (CompRHS (Comp s1 op s2))) c = do
  s1' <- readS s1 c
  s2' <- readS s2 c
  nextInstWX x (if cmp op s1' s2' then 1 else 0) c
step (Assign x1 (MemRead (MemLoc x2 offset))) c = do
  index <- (fromIntegral offset +) <$> readX x2 c
  nextInstWX x1 (readMem "step MemRead" index c) c
step (Assign x (Allocate size datum)) c   = do
  size'  <- readS size c
  datum' <- readS datum c
  let (h, c') = allocate size' datum' c
  nextInstWX x h c'
step (Assign x (Print s)) c = do
  s' <- readS s c
  nextInstWX x 1 $ print s'  c
step (Assign _ (ArrayError s1 s2)) c = do
  s1' <- readS s1 c
  s2' <- readS s2 c
  return $ arrayError s1' s2' c
step (Assign x (SRHS s)) c = do
  s' <- readS s c
  nextInstWX x s' c
-- Math Inst
step (MathInst x op s) c = do 
  x' <- readX x c
  s' <- readS s c
  nextInstWX x (runOp op x' s') c
-- CJump
step (CJump (Comp s1 op s2) l1 l2) c = do
  s1' <- readS s1 c
  s2' <- readS s1 c
  return $ goto (findLabelIndex (if cmp op s1' s2' then l1 else l2) c) c
-- MemWrite
step (MemWrite (MemLoc x offset) s) c = do
  index <- (fromIntegral offset +) <$> readX x c
  s' <- readS s c
  return . nextInst $ writeMem "step MemWrite" index s' c
-- Goto
step (Goto l) c = return $ goto (findLabelIndex l c) c
-- LabelDec, just advance
step (LabelDeclaration _) c = return $ nextInst c
-- Call
step (Call s) c = do
  func <- readS s c
  addEnv $ Map.empty
  let c' = push (c^.ip + 1) c
  return $ goto func c'
-- TailCall
step (TailCall s) c = goto <$> readS s c <*> return c
-- Return
step Return c =
  let rspVal = readReg rsp c
      done   = rspVal >= (fromIntegral $ Vector.length (c^.memory) * 8)
      c'     = writeReg rsp (rspVal + 8) c
      c''    = goto (readMem "step Return" rspVal c') c'
      f :: Bool -> [Env] -> (NonEmpty Env, Computer L2Instruction)
      -- in this case, we must be returning from main
      -- the computer thinks it's finished
      -- there are no environments left
      -- it's ok to halt the computer
      f True  []     = (Map.empty :| [], halt c) 
      -- in this case we are trying to return
      -- an we think the computer is finished, 
      -- but there are extra environments remaining
      -- there must be some programming error
      f True  (x:xs) = error $ "computer finished, but multiple environments exist" ++ show (x:xs)
      -- we are returning, and the computer is not halted
      -- so check if there are enough environments remaining, and pop the top one.
      f False (x':x'':xs) = (x'':|xs, c'')
      -- here we are trying to return
      -- and there are no more environments
      -- but the computer isnt in a finished state...
      -- there must be some programming error
      f False []     = error $ "trying to return, but no environments remaining"
  in do 
    es <- tail <$> get
    let (newEs,c) = f done es
    put newEs
    return c

readS :: L2S -> Computer L2Instruction -> M Int64
readS (NumberL2S n) _ = return n
readS (XL2S x)      c = readX x c
readS (LabelL2S l)  c = return $ findLabelIndex l c

readX :: L2X -> Computer L2Instruction -> M Int64
readX (RegL2X r) c = return $ readReg r c
readX (VarL2X v) _ = 
  do e <- head <$> get; return $ fromMaybe (unbound v) (Map.lookup v e)

unbound v = error $ "unbound variable: " ++ v

writeX :: L2X -> Int64 -> Computer L2Instruction -> M (Computer L2Instruction)
writeX (RegL2X r) i c = return $ writeReg r i c
writeX (VarL2X v) i c = do e <- head <$> get; replaceHead $ Map.insert v i e; return c

replaceHead :: Env -> M ()
replaceHead e = do es <- get; put $ e :| tail es

addEnv :: Env -> M ()
addEnv e = do es <- get; put $ cons e es

-- goto the next instruction after writing an x value
nextInstWX :: L2X -> Int64 -> Computer L2Instruction -> M (Computer L2Instruction)
nextInstWX x i c = nextInst <$> writeX x i c
