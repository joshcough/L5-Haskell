module L.L1.L1Interp (
  Computer(..)
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

data Computer = Computer {
  registers :: RegisterState,
  memory    :: Memory,
  output    :: Output,
  ip        :: Ip,
  heapP     :: Int, -- pointer to top of heap
  halted    :: Bool
} deriving (Show)

showOutput cs = mkString "" $ reverse (output cs)

oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 4 --twoMeg
ebpStart = (memSize - 1) * 4
espStart = ebpStart
registerStartState = Map.fromList [(eax, 0), (ebx, 0), (ecx, 0), (edx, 0),
                                   (edi, 0), (esi, 0), (ebp, ebpStart), (esp, espStart)]
emptyMem = Vector.replicate memSize 0
emptyState = Computer registerStartState emptyMem [] 0 0 False

-- advance the computer to the next instruction
nextInst :: Computer -> Computer
nextInst cs = goto ((ip cs) + 1) cs

-- set the value of a register to an int value
writeReg :: Register -> Int -> Computer -> Computer
writeReg reg newValue cs = newReg (Map.insert reg newValue $ registers cs) cs

-- set the value of r1 to the value of r2
set :: Register -> Register -> Computer -> Computer
set r1 r2 cs = writeReg r1 (readReg r2 cs) cs

-- read the value of a register
readReg :: Register -> Computer -> Int
readReg r cs = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ registers cs)

-- write an int into memory at the given address
writeMem :: Int -> Int -> Computer -> Computer
writeMem addr value cs = newMem (memory cs Vector.// [(addr `div` 4, value)]) cs

-- read a single int from memory
readMem :: Int -> Computer -> Int
readMem addr cs = memory cs Vector.! (addr `div` 4)

-- read an array from memory
readArray :: Int -> Computer -> Vector Int
readArray addr cs = 
  let size = readMem addr cs
  in Vector.slice (addr `div` 4 + 1) size (memory cs)

-- push the given int argument onto the top of the stack
-- adjust esp accordingly
push :: Int -> Computer -> Computer
push value cs =
  let espVal = readReg esp cs
      newMem = writeMem espVal value cs
  in writeReg esp (espVal - 4) newMem

-- pop the top value off the stack into the given register
-- adjust esp accordingly.
pop :: Register -> Computer -> Computer
pop r cs =
  let espVal   = readReg esp cs
      newState = writeReg r (readMem espVal cs) cs
  in writeReg esp (espVal + 4) newState

adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: Int -> Int -> Computer -> (Int, Computer)
allocate size n cs =
  let size'   = adjustNum size
      ns      = Prelude.replicate size' n
      indices = [(heapP cs `div` 4)..]
      heap    = newMem (memory cs Vector.// (zip indices (size' : ns)))
                       (bumpHeap ((size'+1)*4) cs)
  in (heapP cs, heap)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
l1print :: Int -> Computer -> Computer
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

-- print an array error
arrayError :: Int -> Int -> Computer -> Computer
arrayError a x cs = halt msg cs where
  pos  = show $ adjustNum x
  size = show $ readMem a cs
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

-- all this should be able to be replaced with Lens
newReg :: RegisterState -> Computer -> Computer
newReg rs cs = Computer rs (memory cs) (output cs) 
                         (ip cs) (heapP cs) (halted cs)
newMem :: Memory -> Computer -> Computer
newMem m cs = Computer (registers cs) m (output cs) 
                        (ip cs) (heapP cs) (halted cs)
goto :: Ip -> Computer -> Computer
goto ip cs = Computer (registers cs) (memory cs) (output cs) 
                       ip (heapP cs) (halted cs)
addOutput :: String -> Computer -> Computer
addOutput s cs = Computer (registers cs) (memory cs) (s : output cs) 
                           (ip cs) (heapP cs) (halted cs)
bumpHeap n cs = Computer (registers cs) (memory cs) (output cs) 
                          (ip cs) (heapP cs + n) (halted cs)
halt msg cs = Computer (registers cs) (memory cs) (msg : output cs)
                        (ip cs) (heapP cs) True

-- run the computer with the given L1 program
interp :: L1 -> Computer
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
  readS :: L1S -> Computer -> Int
  readS (NumberL1S n) _ = n
  readS (RegL1S r)   cs = readReg r cs
  readS (LabelL1S l)  _ = findLabelIndex l
  advance :: Computer -> Computer
  advance = nextInst |> go
  advanceWR :: Register -> Int -> Computer -> Computer
  advanceWR r i cs = advance $ writeReg r i cs
  -- the main loop
  go :: Computer -> Computer
  go cs = f (prog Vector.! (ip cs)) (traceA cs) where
    f :: L1Instruction -> Computer -> Computer
     -- Assignment statements
    f (Assign r (CompRHS (Comp s1 op s2))) cs  = advanceWR r 
      (if (cmp op (readS s1 cs) (readS s2 cs)) then 1 else 0) cs
    f (Assign r (MemRead (MemLoc x offset))) cs =
      let index = (readReg x cs) + offset
      in advanceWR r (readMem index cs) cs
    f (Assign r (Allocate s1 s2)) cs   = 
      let (h, cs') = allocate (readS s1 cs) (readS s2 cs) cs
      in advanceWR r h cs'
    f (Assign r (Print s)) cs          = 
      advanceWR r 1 $ l1print  (readS s cs)  cs
    f (Assign r (ArrayError s1 s2)) cs = 
      arrayError (readS s1 cs) (readS s2 cs) cs
    f (Assign r (SRHS s)) cs           = 
      advanceWR r (readS s cs) cs
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
          newStack = push (readReg ebp cs) cs -- pushl %ebp
          newEbp   = set ebp esp newStack     -- movl %esp, %ebp
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
