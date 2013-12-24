module L.L1.L1Interp (CompState(..), interp, interpL1File) where

import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Tuple
import Data.Map (Map)
import qualified Data.Map as Map
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils
import Debug.Trace

type RegisterState = Map Register Int
type Memory = Vector Int
type Output = [String]
type Ip = Int -- instruction pointer

data CompState = CompState {
  registers :: RegisterState,
  memory    :: Memory,
  output    :: Output,
  ip        :: Ip
} deriving (Show)

--instance Show CompState where
--  show cs = show (output cs)

oneMeg = 1048576
twoMeg = oneMeg * 2
--memSize = twoMeg
memSize = 16
ebpStart = memSize - 1
espStart = ebpStart
registerStartState = Map.fromList [(eax, 0), (ebx, 0), (ecx, 0), (edx, 0),
                                   (edi, 0), (esi, 0), (ebp, ebpStart), (esp, espStart)]
emptyMem = Vector.replicate memSize 0
emptyState = CompState registerStartState emptyMem [] 0

nextInst :: CompState -> CompState
nextInst cs = goto ((ip cs) + 1) cs

writeReg :: Register -> Int -> CompState -> CompState
writeReg reg newValue cs = newReg (Map.insert reg newValue $ registers cs) cs

writeMem :: Int -> Int -> CompState -> CompState
writeMem index value cs = newMem (memory cs Vector.// [(index, value)]) cs

set :: Register -> Register -> CompState -> CompState
set r1 r2 cs = writeReg r1 (readReg r2 cs) cs

push :: Int -> CompState -> CompState
push value cs =
  let espVal = readReg esp cs
      newMem = writeMem espVal value cs
  in writeReg esp (espVal - 1) newMem

pop :: Register -> CompState -> CompState
pop r cs =
  let espVal   = readReg esp cs
      newState = writeReg r (readMem espVal cs) cs
  in writeReg esp (espVal + 1) newState

readReg :: Register -> CompState -> Int
readReg r cs = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ registers cs)

readMem :: Int -> CompState -> Int
readMem i cs = memory cs Vector.! i

newReg :: RegisterState -> CompState -> CompState
newReg rs cs = CompState rs (memory cs) (output cs) (ip cs)
newMem :: Memory -> CompState -> CompState
newMem m cs = CompState (registers cs) m (output cs) (ip cs)
goto :: Ip -> CompState -> CompState
goto ip cs = CompState (registers cs) (memory cs) (output cs) ip
addOutput :: String -> CompState -> CompState
addOutput s cs = CompState (registers cs) (memory cs) (s : output cs) (ip cs)

interp :: L1 -> CompState
interp (Program main fs) = 
  let -- put all the instructions into a single vector
      insts = Prelude.concat $ (body main) : (fmap body fs) ++ [[Return]]
      prog :: Vector L1Instruction
      prog = Vector.fromList insts
      instructionsWithIndex = zipWithIndex insts
      indeces :: Map L1Instruction Int
      indeces = Map.fromList instructionsWithIndex
      findLabelIndex :: Label -> Int
      findLabelIndex l =
        maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) indeces)
      go :: CompState -> CompState
      go cs = 
        let i = prog Vector.! (ip cs) 
            advance :: CompState -> CompState
            advance = nextInst |> go
            advanceWR :: Register -> Int -> CompState -> CompState
            advanceWR r i cs = advance $ writeReg r i cs
            getSValue :: L1S -> CompState -> Int
            getSValue (NumberL1S n) _ = n
            getSValue (RegL1S r)   cs = readReg r cs
            getSValue (LabelL1S l)  _ = findLabelIndex l
            f :: L1Instruction -> CompState -> CompState
            -- Assignment statements
            f (Assign r (CompRHS (Comp s1 op s2))) cs = advanceWR r 
              (if (cmp op (getSValue s1 cs) (getSValue s1 cs)) then 1 else 0) cs
            f (Assign r (Allocate s1 s2)) cs   = advanceWR r (error "todo, allocate") cs
            f (Assign r (Print s)) cs          = advanceWR r 0 (addOutput (show $ getSValue s cs) cs)  
            f (Assign r (ArrayError s1 s2)) cs = advanceWR r (error "todo, arrayerror") cs
            f (Assign r (MemRead loc)) cs      = advanceWR r (error "todo, memread") cs
            f (Assign r (SRHS s)) cs           = advanceWR r (getSValue s cs) cs
            -- Math Inst
            f (MathInst r op s) cs = advance $ 
              writeReg r (runOp op (readReg r cs) (getSValue s cs)) cs
            -- CJump
            f (CJump (Comp s1 op s2) l1 l2) cs = 
              let l = if cmp op (getSValue s1 cs) (getSValue s2 cs) 
                      then findLabelIndex l1 else findLabelIndex l2
              in go $ goto l cs
            -- MemWrite
            f (MemWrite (MemLoc x offset) s) cs =
              let index = (readReg x cs) + (offset `div` 4)
              in writeMem index (getSValue s cs) cs
            -- Goto
            f (Goto l) cs = go $ goto (findLabelIndex l) cs
            -- LabelDec, just advance
            f (LabelDeclaration _) cs = advance cs
            -- Call
            f (Call s) cs = 
              let func       = getSValue s cs
                  newStack   = push (readReg ebp cs) cs
                  newEbp     = set ebp esp newStack
                  res        = go $ goto func newEbp
              -- TODO: what if calling s threw an error?
              in go $ goto (ip cs + 1) res
            -- TailCall
            f (TailCall s) cs = go $ goto (getSValue s cs) cs
            -- Return
            f Return cs = pop ebp $ set esp ebp cs 
        in f (traceA i) (traceA cs)
  in go emptyState -- starts go at instruction 0

interpL1File = 
  do s <- compile_ $ (either error id) . interpL1
     putStrLn (snd s) where
  interpL1 code = show . interp <$> parseL1 (sread code)
