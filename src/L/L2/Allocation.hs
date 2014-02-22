{-# LANGUAGE TupleSections #-}
module L.L2.Allocation (allocate) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import L.L1L2AST
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.L2.Vars

-- gives back a fully allocated function (if its possible to allocate it)
-- with all of the variables replaced with the assigned registers.
allocate :: L2Func -> L1Func
allocate f = Func finalL1Body where
  {-
    Allocate completely gives us back
      - An list of L2 instructions
          This list is semantically equivalent to the original function
          However, it might contain many 'spilled variables', and so
          the body of the function might differ dramatically from the original.
      - A map from variables to registers
          This represents where those variables will live.
      - The number of variables in the original function that were spilled.
   -}
  (allocatedBody, allocs, nrVarsSpilled) = allocateCompletely (body f)
  {-
    L2 requires that all functions have labels (names), except for main.
    Later, when decrement the stack, we need it to come after the label.
    Adding the main label makes this process easier, although it isn't
    really necessary.
   -}
  label = case allocatedBody !! 0 of
    l@(LabelDeclaration x) -> l
    _ -> LabelDeclaration "main"

  {-
    Make sure the stack is always aligned to 16 bytes, which is an x86-64 requirement.
    on any call to a function, an 8 byte address is put on the stack.
    so right off the bat we need to decrement the stack pointer by 8.
    after that, the number of vars spilled determines how much more stack space we use.
    currently, 8 bytes is used for each variable, L2 doesn't yet support any data types
    (hopefully that will change in the future)
    so, to keep the stack 16 byte aligned under the current scheme
    if the number of vars spilled is odd, we have to decrement by 8 more bytes.
   -}
  offset = 8 + (if even nrVarsSpilled then nrVarsSpilled * 8 else nrVarsSpilled * 8 + 8)
  decEsp = MathInst (RegL2X rsp) decrement (NumberL2S (fromIntegral offset))
  incEsp = MathInst (RegL2X rsp) increment (NumberL2S (fromIntegral offset))

  {-
    whenever we return, we must make sure to put the stack pointer back.
    (by incrementing it by the amount we decremented it by.)
    rewriteReturns puts that increment in front of all returns in the function.
   -}
  rewriteReturns insts = insts >>= f where
    f r@Return = [incEsp, r]
    f i        = [i]

  {-
    Here, we put all the parts together into one function
      its label
      the stack decremented
      then the rest of the instructions (with stack increments put before all the returns)
   -}
  finalL2Body = concat [[label], [decEsp], rewriteReturns $ tail allocatedBody]

  {- Finally, we replace all the variables with the registers they will live in. -}
  finalL1Body = replaceVarsWithRegisters allocs finalL2Body


allocateCompletely :: [L2Instruction] -> ([L2Instruction], Map Variable Register, Int)
allocateCompletely body = let
  finalState = runState (go 0 body) 0
  -- TODO: the second thing is the offset...
  -- but it probably has to be adjusted somehow
  in (fst $ fst finalState, snd $ fst finalState, snd finalState) where

    go :: Int -> [L2Instruction] -> State Int ([L2Instruction], Map Variable Register)
    go offset insts =
      let (Interference g) = buildInterferenceGraph $ liveness insts
      in case attemptAllocation (Interference g) of
        Just registerMap -> return (insts, registerMap)
        Nothing ->
          -- find the next variable to spill by figuring out which one has the most connections
          -- TODO: tie should go to the one with the longest liverange.
          let vs = Set.toList $ vars g
              s (v1, n1) (v2, n2) = compare n1 n2
              f (v, _) = not $ isPrefixOf defaultSpillPrefix v
              conns = filter f $ reverse $ sortBy s $ fmap (\v -> (v, Set.size $ connections (VarL2X v) g)) vs
              v = fst $ head conns
          in spillDef (v, offset * 8) insts >>= go (offset + 1)

{-
  OLD STUFF THAT USED foldM.
  --nrVarsToSpill = length varsToSpill
  --rspOffset = (- nrVarsToSpill * 8) - (if even nrVarsToSpill then 0 else 8)
  --finalState = runState (foldM (flip spillDef) (body originalF) (zipWithIndex varsToSpill)) 0
  --finalBody = fst runState
  --finalOffset = snd runState
  --g = buildInterferenceGraph $ liveness finalBody
  --in case attemptAllocation g of
  --  Just registerMap -> traceA ((Func newBody, registerMap), rspOffset)
  --  Nothing -> error "allocation impossible"
 -}

-- TODO: this stuff should be someplace better than this
regSet = Set.fromList allocatableRegisters
getRegisters s = Set.fromList [ r | RegL2X r <- Set.toList s ]

attemptAllocation :: Interference -> Maybe (Map Variable Register)
attemptAllocation i@(Interference g) = let
    vs = vars g
    pairings :: Map Variable Register
    pairings = foldl (findMatch g) Map.empty (Set.toList vs) -- TODO: sort list here
    unpairedVars :: Set Variable
    unpairedVars = vs Set.\\ Map.keysSet pairings
  in if Set.null unpairedVars then Just pairings else Nothing

-- try to assign a register to a variable
--   if its not possible, just return the input map
--   if it is, return the input map + (v, r)
findMatch :: InterferenceGraph -> Map Variable Register -> Variable -> Map Variable Register
findMatch g pairs v = maybe pairs (\r -> Map.insert v r pairs) choice where
  -- everything v conflicts with (reisters and variables)
  conflicts    :: Set L2X
  conflicts    = connections (_Variable # v) g
  -- just the variables v conflicts with
  varConflicts :: Set Variable
  varConflicts = vars conflicts
  -- just the registers v conflicts with
  regConflicts :: Set Register
  regConflicts = getRegisters $ conflicts Set.\\ Set.map VarL2X varConflicts
  -- all the registers v does NOT conflict with
  nonConflictingRegisters :: Set Register
  nonConflictingRegisters =  regSet Set.\\ regConflicts
  -- the set of registers that are already
  -- taken by variables that v conflicts with
  registersConflictingVarsLiveIn :: Set Register
  registersConflictingVarsLiveIn = Set.fromList $ do
    v <- Set.toList varConflicts
    maybe [] (\a -> [a]) $ Map.lookup v pairs
  -- the available registers come from taking
  -- the registers it doesnt conflict with, and removing those
  -- that are already taken by variables v conflicts with
  -- this set might be empty, meaning there is no register for v.
  --   (also meaning the graph is not colorable)
  availableRegisters :: Set Register
  availableRegisters = nonConflictingRegisters Set.\\ registersConflictingVarsLiveIn
  -- choose a register for v, if one is available.
  choice :: Maybe Register
  choice = listToMaybe $ Set.toList availableRegisters -- TODO: sort list here.

-- replaces variables with registers in a list of L2 instructions.
-- this turns them into L1 instructions.
replaceVarsWithRegisters :: Map Variable Register -> [L2Instruction] -> [L1Instruction]
replaceVarsWithRegisters replacements insts = fmap replaceInInst insts where
  replaceInInst :: L2Instruction -> L1Instruction
  replaceInInst (Assign x rhs)        = Assign (getRegister x) (replaceInRHS rhs)
  replaceInInst (MathInst x op s)     = MathInst (getRegister x) op (replaceInS s)
  replaceInInst (MemWrite   loc s)    = MemWrite (replaceInMemLoc loc) (replaceInS s)
  replaceInInst (Goto s)              = Goto s
  replaceInInst (CJump comp l1 l2)    = CJump (replaceInComp comp) l1 l2
  replaceInInst (Call s)              = Call $ replaceInS s
  replaceInInst (TailCall s)          = TailCall $ replaceInS s
  replaceInInst (LabelDeclaration ld) = LabelDeclaration ld
  replaceInInst Return                = Return

  replaceInS :: L2S -> L1S
  replaceInS (XL2S (VarL2X v)) = maybe (error "bad register") RegL1S $ Map.lookup v replacements
  replaceInS (XL2S (RegL2X r)) = RegL1S r
  replaceInS (NumberL2S n)     = NumberL1S n
  replaceInS (LabelL2S n)      = LabelL1S n

  replaceInRHS :: AssignRHS L2X L2S -> AssignRHS L1X L1S
  replaceInRHS (Allocate s1 s2)       = Allocate (replaceInS s1) (replaceInS s2)
  replaceInRHS (Print s)              = Print (replaceInS s)
  replaceInRHS (ArrayError s1 s2)     = ArrayError (replaceInS s1) (replaceInS s2)
  replaceInRHS (MemRead loc)          = MemRead (replaceInMemLoc loc)
  replaceInRHS (SRHS s)               = SRHS (replaceInS s)
  replaceInRHS (CompRHS comp)         = CompRHS (replaceInComp comp)

  replaceInMemLoc (MemLoc x off) = MemLoc (getRegister x) off
  replaceInComp (Comp s1 op s2)  = Comp   (replaceInS s1) op (replaceInS s2)

  getRegister :: L2X -> Register
  getRegister (VarL2X v) = maybe (error "bad register") id $ Map.lookup v replacements
  getRegister (RegL2X r) = r
