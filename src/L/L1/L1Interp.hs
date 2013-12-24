module L.L1.L1Interp (
  CompState(..)
 ,interp
 ,interpL1File
 ,interpL1OrDie
 ) where

import Control.Applicative
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils

type RegisterState = Map Register Int
type Memory = Vector Int
type Output = [String]
type Ip = Int -- instruction pointer

data CompState = CompState {
  registers :: RegisterState,
  memory    :: Memory,
  output    :: Output,
  ip        :: Ip,
  heapP     :: Int -- pointer to top of heap
} deriving (Show)

showOutput cs = mkString "" $ reverse (output cs)

oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 32 --2048 --twoMeg
ebpStart = (memSize - 1) * 4
espStart = ebpStart
registerStartState = Map.fromList [(eax, 0), (ebx, 0), (ecx, 0), (edx, 0),
                                   (edi, 0), (esi, 0), (ebp, ebpStart), (esp, espStart)]
emptyMem = Vector.replicate memSize 0
emptyState = CompState registerStartState emptyMem [] 0 0

nextInst :: CompState -> CompState
nextInst cs = goto ((ip cs) + 1) cs

writeReg :: Register -> Int -> CompState -> CompState
writeReg reg newValue cs = newReg (Map.insert reg newValue $ registers cs) cs

set :: Register -> Register -> CompState -> CompState
set r1 r2 cs = writeReg r1 (readReg r2 cs) cs

readReg :: Register -> CompState -> Int
readReg r cs = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ registers cs)

writeMem :: Int -> Int -> CompState -> CompState
writeMem index value cs = newMem (memory cs Vector.// [(index `div` 4, value)]) cs

readMem :: Int -> CompState -> Int
readMem i cs = memory cs Vector.! (i `div` 4)

readArray :: Int -> CompState -> Vector Int
readArray i cs = 
  let size = readMem i cs
  in Vector.slice (i `div` 4 + 1) size (memory cs)

push :: Int -> CompState -> CompState
push value cs =
  let espVal = readReg esp cs
      newMem = writeMem espVal value cs
  in writeReg esp (espVal - 4) newMem

pop :: Register -> CompState -> CompState
pop r cs =
  let espVal   = readReg esp cs
      newState = writeReg r (readMem espVal cs) cs
  in writeReg esp (espVal + 4) newState

adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: Int -> Int -> CompState -> (Int, CompState)
allocate size n cs =
  let size'   = adjustNum size
      ns      = Prelude.replicate size' n
      indices = [(heapP cs `div` 4)..]
      heap    = newMem (memory cs Vector.// (zip indices (size' : ns))) (bumpHeap ((size'+1)*4) cs)
  in (heapP cs, heap)

l1print :: Int -> CompState -> CompState
l1print n cs = addOutput (printContent n 0 ++ "\n") cs where
  printContent :: Int -> Int -> String
  printContent n depth
    | depth >= 4   = "..."
    | n .&. 1 == 1 = show $ shiftR n 1
    | otherwise    =
      let size  = readMem n cs
          arr   = readArray n cs
          contentsV = Vector.map (\n -> printContent n $ depth + 1) arr
          contents = mkString ", " $ show size : Vector.toList contentsV
      in "{s:" ++ contents ++ "}"

-- all this should be able to be replaced with Lens
newReg :: RegisterState -> CompState -> CompState
newReg rs cs = CompState rs (memory cs) (output cs) (ip cs) (heapP cs)
newMem :: Memory -> CompState -> CompState
newMem m cs = CompState (registers cs) m (output cs) (ip cs) (heapP cs)
goto :: Ip -> CompState -> CompState
goto ip cs = CompState (registers cs) (memory cs) (output cs) ip (heapP cs)
addOutput :: String -> CompState -> CompState
addOutput s cs = CompState (registers cs) (memory cs) (s : output cs) (ip cs) (heapP cs)
bumpHeap n cs = CompState (registers cs) (memory cs) (output cs) (ip cs) (heapP cs + n)

interp :: L1 -> CompState
interp (Program main fs) = go emptyState where -- starts go at instruction 0 
  -- put all the instructions into a single vector
  insts = Prelude.concat $ (body main ++ [Return]) : (fmap body fs)
  prog :: Vector L1Instruction
  prog = Vector.fromList insts
  indeces :: Map L1Instruction Int
  indeces = Map.fromList $ zipWithIndex insts
  findLabelIndex :: Label -> Int
  findLabelIndex l =
    maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) indeces)
  readS :: L1S -> CompState -> Int
  readS (NumberL1S n) _ = n
  readS (RegL1S r)   cs = readReg r cs
  readS (LabelL1S l)  _ = findLabelIndex l
  advance :: CompState -> CompState
  advance = nextInst |> go
  advanceWR :: Register -> Int -> CompState -> CompState
  advanceWR r i cs = advance $ writeReg r i cs
  go :: CompState -> CompState
  go cs = f (prog Vector.! (ip cs)) cs where
    f :: L1Instruction -> CompState -> CompState
     -- Assignment statements
    f (Assign r (CompRHS (Comp s1 op s2))) cs  = advanceWR r 
      (if (cmp op (readS s1 cs) (readS s2 cs)) then 1 else 0) cs
    f (Assign r (MemRead (MemLoc x offset))) cs =
      let index = (readReg x cs) + offset
      in advanceWR r (readMem index cs) cs
    f (Assign r (Allocate s1 s2)) cs   = 
      let (h, cs') = allocate (readS s1 cs) (readS s2 cs) cs
      in advanceWR r h cs'
    f (Assign r (Print s)) cs          = advanceWR r 1 $ l1print (readS s cs) cs
    f (Assign r (ArrayError s1 s2)) cs = error "TODO: arrayerror"
    f (Assign r (SRHS s)) cs           = advanceWR r (readS s cs) cs
    -- Math Inst
    f (MathInst r op s) cs = advance $ 
      writeReg r (runOp op (readReg r cs) (readS s cs)) cs
    -- CJump
    f (CJump (Comp s1 op s2) l1 l2) cs = 
      let l = if cmp op (readS s1 cs) (readS s2 cs) 
              then findLabelIndex l1 else findLabelIndex l2
      in go $ goto l cs
    -- MemWrite
    f (MemWrite (MemLoc x offset) s) cs =
      let index = (readReg x cs) + offset
      in advance $ writeMem index (readS s cs) cs
    -- Goto
    f (Goto l) cs = go $ goto (findLabelIndex l) cs
    -- LabelDec, just advance
    f (LabelDeclaration _) cs = advance cs
    -- Call
    f (Call s) cs  = 
      let func     = readS s cs
          newStack = push (readReg ebp cs) cs
          newEbp   = set ebp esp newStack
          res      = go $ goto func newEbp
      -- TODO: what if calling s threw an error?
      in go $ goto (ip cs + 1) res
    -- TailCall
    f (TailCall s) cs = go $ goto (readS s cs) cs
    -- Return
    f Return cs = pop ebp $ set esp ebp cs 

interpL1 :: String -> Either String String
interpL1 code = showOutput . interp <$> parseL1 (sread code)

interpL1OrDie :: String -> String
interpL1OrDie = (either error id) . interpL1

interpL1File = 
  do s <- compile_ $ (either error id) . interpL1
     putStrLn (snd s)
